{-# LANGUAGE TupleSections #-}
module Baerbel.Runtime.Asterisk (InstallStatus(..), adopt, preview, previewSite, install, installSite, restore) where

import Baerbel.Core.Accounts (validId)
import Baerbel.Core.Asterisk
import Baerbel.Core.Error (Result, failure)
import Baerbel.Runtime.Effects (continue, mapResult)
import Baerbel.Runtime.Error (ioResult)
import Baerbel.Runtime.Storage (atomicWrite, databaseResult)
import Control.Monad (filterM, unless, void, when)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Bytes8
import Data.Char (isDigit)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Database.SQLite.Simple (Connection, Only(..), Query, execute, execute_, query_, withConnection, withImmediateTransaction)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeFile, removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

adopt :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Result ())
adopt root routePath config database = runExceptT $ do
  legacy <- ExceptT (ioResult "asterisk_routes_unavailable" (Text.readFile routePath))
  (shared, dedicated, previous) <- except (legacyRoutes legacy)
  names <- ExceptT (ioResult "asterisk_configuration_unavailable" (listDirectory root))
  files <- ExceptT (ioResult "asterisk_configuration_unavailable" (readManaged root names))
  (pbx, standard, pro) <- except (capture root previous "stefan" shared dedicated files)
  ExceptT (writeSip database previous standard pro)
  ExceptT (atomicWrite config (Yaml.encode pbx))

legacyRoutes :: Text -> Result (Text, Text, Text)
legacyRoutes input = case Text.words input of
  [standard, pro, ident] | validPhone standard && validPhone pro && validId ident -> Right (standard, pro, ident)
  _ -> Left (failure "invalid_legacy_routes")

validPhone :: Text -> Bool
validPhone phone = Text.length phone == 13 && Text.isPrefixOf "+44" phone
  && Text.all isDigit (Text.drop 3 phone)

readManaged :: FilePath -> [FilePath] -> IO (Map FilePath Text)
readManaged root names = Map.traverseWithKey (const Text.readFile) sources
  where
    selected name = fmap (,source) (managedName name)
      where source = root </> name
    sources = Map.fromList (mapMaybe selected (filter (isSuffixOf ".conf") names))

writeSip :: FilePath -> Text -> Sip -> Sip -> IO (Result ())
writeSip database previous standard pro = databaseResult "sip_database_unavailable" (withConnection database save)
  where
    save connection = execute_ connection "PRAGMA foreign_keys=ON" >> withImmediateTransaction connection (migrate connection)
    migrate connection = do
      execute_ connection "PRAGMA defer_foreign_keys=ON"
      execute connection "UPDATE users SET id='stefan' WHERE id=? AND user_type='Pro'" (Only previous)
      mapM_ (\sql -> execute connection sql (Only previous)) renameStatements
      execute_ connection "CREATE TABLE IF NOT EXISTS sip_accounts (number TEXT PRIMARY KEY, username TEXT NOT NULL, password TEXT NOT NULL, pro_user_id TEXT UNIQUE REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE) WITHOUT ROWID"
      execute_ connection "CREATE UNIQUE INDEX IF NOT EXISTS one_shared_sip ON sip_accounts ((1)) WHERE pro_user_id IS NULL"
      execute_ connection "CREATE VIEW IF NOT EXISTS user_sip_accounts AS SELECT users.id AS user_id, sip_accounts.number AS sip_number FROM users JOIN sip_accounts ON (users.user_type='Standard' AND sip_accounts.pro_user_id IS NULL) OR (users.user_type='Pro' AND sip_accounts.pro_user_id=users.id)"
      insertSip connection standard Nothing
      insertSip connection pro (Just "stefan")

renameStatements :: [Query]
renameStatements =
  [ "UPDATE user_settings SET user_id='stefan' WHERE user_id=?"
  , "UPDATE user_memory SET user_id='stefan' WHERE user_id=?"
  , "UPDATE google_grants SET user_id='stefan' WHERE user_id=?"
  , "UPDATE google_grant_scopes SET user_id='stefan' WHERE user_id=?"
  , "UPDATE google_connections SET user_id='stefan' WHERE user_id=?"
  , "UPDATE delivery_receipts SET user_id='stefan' WHERE user_id=?"
  , "UPDATE sms_receipts SET user_id='stefan' WHERE user_id=?"
  , "UPDATE event_cursors SET user_id='stefan' WHERE user_id=?" ]

insertSip :: Connection -> Sip -> Maybe Text -> IO ()
insertSip connection line owner = execute connection "INSERT INTO sip_accounts VALUES (?,?,?,?)" values
  where values = (sipNumber line, sipUser line, sipPassword line, owner)

readRoster :: FilePath -> IO (Result Roster)
readRoster path = mapResult buildRoster (databaseResult "sip_database_unavailable" (withConnection path fetch))
  where
    fetch connection = (,) <$> query_ connection "SELECT number, username, password FROM sip_accounts WHERE pro_user_id IS NULL"
      <*> query_ connection "SELECT users.id, users.phone, number, username, password FROM users LEFT JOIN sip_accounts ON users.id=pro_user_id WHERE user_type='Pro'"

buildRoster :: ([(Text, Text, Text)], [(Text, Text, Maybe Text, Maybe Text, Maybe Text)]) -> Result Roster
buildRoster ([(number, user, secret)], dedicated) = Roster <$> sip number user secret <*> (Map.fromList <$> traverse row dedicated)
  where
    row (ident, phone, Just did, Just name, Just password) | validId ident && validPhone phone = siteLine <$> sip did name password
      where siteLine line = (ident, ProLine line phone)
    row _ = Left (failure "pro_sip_account_required")
buildRoster _ = Left (failure "shared_sip_account_required")

sip :: Text -> Text -> Text -> Result Sip
sip number user secret
  | validPhone number && all singleLine [user, secret] = Right (Sip number user secret)
  | otherwise = Left (failure "invalid_sip_account")
  where singleLine value = not (Text.null value || Text.any (\char -> char == '\n' || char == '\r') value)

readYaml :: Yaml.FromJSON a => FilePath -> IO (Result a)
readYaml config = mapResult decode loaded
  where
    loaded = ioResult "asterisk_yaml_unavailable" (Yaml.decodeFileEither config)
    decode = either (const (Left (failure "invalid_asterisk_yaml"))) Right

data Plan = Plan
  {planDirectory :: FilePath, planFiles :: Map FilePath Text, planParents :: Map FilePath Bytes.ByteString
  , planEndpoints :: [String]}

readPlan :: FilePath -> FilePath -> IO (Result Plan)
readPlan config database = runExceptT $ do
  pbx <- ExceptT (readYaml config)
  roster <- ExceptT (readRoster database)
  files <- except (render pbx roster)
  let root = pbxDirectory pbx
      normalize _ = includePaths (Map.keys files)
  ExceptT (completePlan root files normalize ["phone-assistant-aa-bristol"])

readSitePlan :: FilePath -> FilePath -> FilePath -> IO (Result Plan)
readSitePlan config database templates = runExceptT $ do
  site <- ExceptT (readYaml config)
  roster <- ExceptT (readRoster database)
  sources <- ExceptT (ioResult "asterisk_templates_unavailable" (readTemplates templates))
  files <- except (renderSite site roster sources)
  let endpoints = map (Text.unpack . proEndpoint) (Map.keys (proSip roster))
  ExceptT (completePlan (siteDirectory site) files (wireModules (Map.keys files)) endpoints)

readTemplates :: FilePath -> IO (Map FilePath Text)
readTemplates root = traverse Text.readFile sources
  where sources = Map.fromList [(name, root </> (name <> ".in")) | name <- templateNames]

completePlan :: FilePath -> Map FilePath Text -> (FilePath -> Text -> Text) -> [String] -> IO (Result Plan)
completePlan root files wire endpoints = do
  modules <- ioResult "asterisk_configuration_unavailable" (readModules root)
  pure (Plan root files . Map.mapMaybeWithKey changed <$> modules <*> pure endpoints)
  where
    changed name old
      | new == old = Nothing
      | otherwise = Just new
      where new = Bytes8.pack (Text.unpack (wire name (Text.decodeLatin1 old)))

preview :: FilePath -> FilePath -> FilePath -> IO (Result ())
preview config database destination = readPlan config database >>= continue (savePlan destination)

previewSite :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Result ())
previewSite config database templates destination = readSitePlan config database templates >>= continue (savePlan destination)

savePlan :: FilePath -> Plan -> IO (Result ())
savePlan destination plan = saveBytes destination (stagedFiles (planFiles plan) (planParents plan))

stagedFiles :: Map FilePath Text -> Map FilePath Bytes.ByteString -> Map FilePath Bytes.ByteString
stagedFiles files parents = Map.union (Map.map Text.encodeUtf8 files) (Map.mapKeys ("parent-" <>) parents)

saveBytes :: FilePath -> Map FilePath Bytes.ByteString -> IO (Result ())
saveBytes folder files = runExceptT $ do
  ExceptT (command "install" ["-d", "-m", "0700", folder])
  ExceptT (fmap sequence_ (Map.traverseWithKey save files))
  where save name = atomicWrite (folder </> name)

data InstallStatus = Updated | Current

install :: FilePath -> FilePath -> FilePath -> IO (Result InstallStatus)
install config database backup = readPlan config database >>= continue (applyPlan backup)

installSite :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Result InstallStatus)
installSite config database templates backup = readSitePlan config database templates >>= continue (applyPlan backup)

applyPlan :: FilePath -> Plan -> IO (Result InstallStatus)
applyPlan backup plan = runExceptT $ do
  current <- ExceptT (ioResult "asterisk_configuration_unavailable" (currentFiles root files))
  ExceptT (update (Map.null parents && current))
  where
    root = planDirectory plan
    files = planFiles plan
    parents = planParents plan
    update True = pure (Right Current)
    update False = runExceptT $ do
      ExceptT idle
      ExceptT (backupPaths root backup (Map.keys files) (Map.keys parents))
      ExceptT (applyOrRestore backup plan)
      pure Updated

currentFiles :: FilePath -> Map FilePath Text -> IO Bool
currentFiles root files = doesDirectoryExist destination >>= compareDirectory
  where
    destination = root </> "phone-assistant"
    names = Map.keys files
    compareDirectory False = pure False
    compareDirectory True = do
      present <- sort <$> listDirectory destination
      compareNames (present == names)
    compareNames False = pure False
    compareNames True = do
      previous <- traverse (doesFileExist . (root </>) . ("phone-assistant-" <>)) names
      same <- Map.traverseWithKey matches files
      pure (not (or previous) && and same)
    matches name expected = do
      content <- Bytes.readFile (destination </> name)
      pure (content == Text.encodeUtf8 expected)

readModules :: FilePath -> IO (Map FilePath Bytes.ByteString)
readModules root = do
  paths <- filterM (doesFileExist . (root </>)) moduleNames
  let sources = Map.fromList [(path, root </> path) | path <- paths]
  traverse Bytes.readFile sources

backupPaths :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> IO (Result ())
backupPaths root backup names parents = runExceptT $ do
  ExceptT (command "install" ["-d", "-m", "0700", backup])
  exists <- ExceptT (ioResult "asterisk_backup_unavailable" (doesDirectoryExist installed))
  let sources = parents ++ map ("phone-assistant-" <>) names ++ ["phone-assistant" | exists]
  ExceptT (copyPresent root backup sources)
  where installed = root </> "phone-assistant"

copyPresent :: FilePath -> FilePath -> [FilePath] -> IO (Result ())
copyPresent from to paths = fmap sequence_ (traverse copy paths)
  where
    copy name = do
      let source = from </> name
      exists <- ioResult "asterisk_backup_unavailable" (present source)
      continue (transfer source) exists
    transfer source True = command "cp" ["-a", "--", source, to]
    transfer _ False = pure (Right ())
    present path = (||) <$> doesFileExist path <*> doesDirectoryExist path

applyOrRestore :: FilePath -> Plan -> IO (Result ())
applyOrRestore backup plan = do
  result <- withSystemTempDirectory "baerbel-asterisk" (installFiles root files (planParents plan))
  archived <- continue (const (removeOld root (Map.keys files))) result
  finished <- continue (const (restart (planEndpoints plan))) archived
  case finished of
    Right () -> pure (Right ())
    Left issue -> restore root backup >> pure (Left issue)
  where
    root = planDirectory plan
    files = planFiles plan

installFiles :: FilePath -> Map FilePath Text -> Map FilePath Bytes.ByteString -> FilePath -> IO (Result ())
installFiles root files parents staging = runExceptT $ do
  let target = root </> "phone-assistant"
  ExceptT (saveBytes staging (stagedFiles files parents))
  ExceptT (ioResult "asterisk_configuration_unavailable" (clearManaged root))
  ExceptT (command "install" ["-d", "-m", "0750", "-g", "asterisk", target])
  ExceptT (fmap sequence_ (traverse (copyManaged target) (Map.keys files)))
  ExceptT (fmap sequence_ (traverse copyParent (Map.keys parents)))
  where
    copyManaged target name = command "install" ["-m", "0640", "-g", "asterisk", staging </> name, target </> name]
    copyParent name = command "dd" ["if=" <> (staging </> ("parent-" <> name)), "of=" <> (root </> name), "status=none"]

removeOld :: FilePath -> [FilePath] -> IO (Result ())
removeOld root names = ioResult "asterisk_archive_unavailable" (mapM_ retire names)
  where
    retire name = do
      let path = root </> ("phone-assistant-" <> name)
      exists <- doesFileExist path
      when exists (removeFile path)

restore :: FilePath -> FilePath -> IO (Result ())
restore root backup = runExceptT $ do
  paths <- ExceptT (ioResult "asterisk_backup_unavailable" (listDirectory backup))
  ExceptT (ioResult "asterisk_restore_unavailable" (clearManaged root))
  ExceptT (copyPresent backup root paths)
  ExceptT (restart [])
clearManaged :: FilePath -> IO ()
clearManaged root = do
  let installed = root </> "phone-assistant"
  exists <- doesDirectoryExist installed
  when exists (removeDirectoryRecursive installed)

idle :: IO (Result ())
idle = mapResult check (run "asterisk" ["-rx", "core show channels concise"])
  where
    check output | null output = Right ()
                 | otherwise = Left (failure "asterisk_has_active_calls")

restart :: [String] -> IO (Result ())
restart endpoints = runExceptT $ do
  ExceptT (command "systemctl" ["restart", "asterisk.service"])
  ExceptT (command "timeout" ["20", "asterisk", "-rx", "core waitfullybooted"])
  report <- ExceptT (run "asterisk" ["-rx", "dialplan show baerbel-account-entry"])
  except (unless ("127.0.0.1:4573/account" `Text.isInfixOf` Text.pack report) (Left (failure "asterisk_account_route_missing")))
  mapM_ (ExceptT . endpoint) ("phone-assistant-aa" : endpoints)
  identifiers <- ExceptT (run "asterisk" ["-rx", "pjsip show identifiers"])
  except (checkIdentifierOrder identifiers)

endpoint :: String -> IO (Result ())
endpoint name = mapResult validate (run "asterisk" ["-rx", "pjsip show endpoint " <> name])
  where
    validate output = unless (any matches (lines output)) (Left (failure "asterisk_sip_endpoint_missing"))
    matches line = case words line of
      "Endpoint:" : ident : _ -> takeWhile (/= '/') ident == name
      _ -> False

checkIdentifierOrder :: String -> Result ()
checkIdentifierOrder output = case dropWhile (not . starts "header") (lines output) of
  _ : remaining | any (starts "ip") remaining -> Right ()
  _ -> Left (failure "asterisk_sip_identifier_order")
  where starts name line = take 1 (words line) == [name]

command :: FilePath -> [String] -> IO (Result ())
command file args = fmap void (run file args)

run :: FilePath -> [String] -> IO (Result String)
run file args = mapResult status (ioResult "asterisk_command_unavailable" (readProcessWithExitCode file args ""))
  where
    status (ExitSuccess, output, _) = Right output
    status _ = Left (failure "asterisk_command_failed")
