module Baerbel.Runtime.Asterisk (adopt, preview, install, restore) where

import Baerbel.Core.Accounts (validId)
import Baerbel.Core.Asterisk
import Baerbel.Core.Error (Result, failure)
import Baerbel.Runtime.Effects (continue, mapResult)
import Baerbel.Runtime.Error (ioResult)
import Baerbel.Runtime.Storage (atomicWrite, databaseResult)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.List (isSuffixOf)
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
  && Text.all (\char -> char >= '0' && char <= '9') (Text.drop 3 phone)

readManaged :: FilePath -> [FilePath] -> IO (Map FilePath Text)
readManaged root names = Map.traverseWithKey (const Text.readFile) sources
  where
    selected name = fmap (\path -> (path, root </> name)) (managedName name)
    sources = Map.fromList (mapMaybe selected (filter (isSuffixOf ".conf") names))

writeSip :: FilePath -> Text -> Sip -> Sip -> IO (Result ())
writeSip database previous standard pro = databaseResult "sip_database_unavailable" (withConnection database save)
  where
    save connection = execute_ connection "PRAGMA foreign_keys=ON" >> withImmediateTransaction connection (migrate connection)
    migrate connection = do
      execute_ connection "PRAGMA defer_foreign_keys=ON"
      execute connection "UPDATE users SET id='stefan' WHERE id=? AND user_type='Pro'" (Only previous)
      mapM_ (\sql -> execute connection sql (Only previous)) renameStatements
      execute_ connection "CREATE TABLE sip_accounts (number TEXT PRIMARY KEY, username TEXT NOT NULL, password TEXT NOT NULL, pro_user_id TEXT UNIQUE REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE) WITHOUT ROWID"
      execute_ connection "CREATE UNIQUE INDEX one_shared_sip ON sip_accounts ((1)) WHERE pro_user_id IS NULL"
      execute_ connection "CREATE VIEW user_sip_accounts AS SELECT users.id AS user_id, sip_accounts.number AS sip_number FROM users JOIN sip_accounts ON (users.user_type='Standard' AND sip_accounts.pro_user_id IS NULL) OR (users.user_type='Pro' AND sip_accounts.pro_user_id=users.id)"
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
    row (ident, phone, Just did, Just name, Just password) | validId ident =
      (\line -> (ident, (phone, line))) <$> sip did name password
    row _ = Left (failure "pro_sip_account_required")
buildRoster _ = Left (failure "shared_sip_account_required")

sip :: Text -> Text -> Text -> Result Sip
sip number user secret
  | validPhone number && all singleLine [user, secret] = Right (Sip number user secret)
  | otherwise = Left (failure "invalid_sip_account")
  where singleLine value = not (Text.null value || Text.any (\char -> char == '\n' || char == '\r') value)

readPlan :: FilePath -> FilePath -> IO (Result (Pbx, Map FilePath Text))
readPlan config database = runExceptT $ do
  decoded <- ExceptT (ioResult "asterisk_yaml_unavailable" (Yaml.decodeFileEither config))
  pbx <- except (either (const (Left (failure "invalid_asterisk_yaml"))) Right decoded)
  roster <- ExceptT (readRoster database)
  files <- except (render pbx roster)
  pure (pbx, files)

preview :: FilePath -> FilePath -> FilePath -> IO (Result ())
preview config database destination = do
  plan <- readPlan config database
  continue (\(_, files) -> saveFiles destination files) plan

saveFiles :: FilePath -> Map FilePath Text -> IO (Result ())
saveFiles folder files = runExceptT $ do
  ExceptT (command "install" ["-d", "-m", "0700", folder])
  ExceptT (fmap sequence_ (Map.traverseWithKey save files))
  where save name = atomicWrite (folder </> name) . Text.encodeUtf8

install :: FilePath -> FilePath -> FilePath -> IO (Result ())
install config database backup = do
  plan <- readPlan config database
  continue (uncurry (applyPlan backup)) plan

applyPlan :: FilePath -> Pbx -> Map FilePath Text -> IO (Result ())
applyPlan backup pbx files = runExceptT $ do
  let root = pbxDirectory pbx
      names = Map.keys files
  ExceptT idle
  parents <- ExceptT (ioResult "asterisk_configuration_unavailable" (readParents root names))
  ExceptT (backupPaths root backup names (Map.keys parents))
  ExceptT (applyOrRestore root backup files parents)

readParents :: FilePath -> [FilePath] -> IO (Map FilePath Text)
readParents root names = do
  paths <- listDirectory root
  let parents = filter (\name -> isSuffixOf ".conf" name && managedName name == Nothing) paths
      sources = Map.fromList [(path, root </> path) | path <- parents]
  original <- Map.traverseWithKey (const Text.readFile) sources
  pure (Map.mapMaybe changed original)
  where
    changed old
      | new == old = Nothing
      | otherwise = Just new
      where new = includePaths names old

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

applyOrRestore :: FilePath -> FilePath -> Map FilePath Text -> Map FilePath Text -> IO (Result ())
applyOrRestore root backup files parents = do
  result <- withSystemTempDirectory "baerbel-asterisk" (installFiles root files parents)
  archived <- continue (const (removeOld root (Map.keys files))) result
  finished <- continue (const restart) archived
  case finished of
    Right () -> pure (Right ())
    Left issue -> restore root backup >> pure (Left issue)

installFiles :: FilePath -> Map FilePath Text -> Map FilePath Text -> FilePath -> IO (Result ())
installFiles root files parents staging = runExceptT $ do
  let target = root </> "phone-assistant"
  ExceptT (saveFiles staging (Map.union files parentsForStage))
  ExceptT (command "install" ["-d", "-m", "0750", "-g", "asterisk", target])
  ExceptT (fmap sequence_ (traverse (copyManaged target) (Map.keys files)))
  ExceptT (fmap sequence_ (traverse copyParent (Map.keys parents)))
  where
    parentsForStage = Map.mapKeys ("parent-" <>) parents
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
  ExceptT (ioResult "asterisk_restore_unavailable" clear)
  ExceptT (copyPresent backup root paths)
  ExceptT restart
  where
    installed = root </> "phone-assistant"
    clear = do
      exists <- doesDirectoryExist installed
      when exists (removeDirectoryRecursive installed)

idle :: IO (Result ())
idle = mapResult check (run "asterisk" ["-rx", "core show channels concise"])
  where
    check output | null output = Right ()
                 | otherwise = Left (failure "asterisk_has_active_calls")

restart :: IO (Result ())
restart = runExceptT $ do
  ExceptT (command "systemctl" ["restart", "asterisk.service"])
  ExceptT (command "timeout" ["20", "asterisk", "-rx", "core waitfullybooted"])
  report <- ExceptT (run "asterisk" ["-rx", "dialplan show baerbel-account-entry"])
  except (unless ("127.0.0.1:4573/account" `Text.isInfixOf` Text.pack report) (Left (failure "asterisk_account_route_missing")))

command :: FilePath -> [String] -> IO (Result ())
command file args = fmap void (run file args)

run :: FilePath -> [String] -> IO (Result String)
run file args = mapResult status (ioResult "asterisk_command_unavailable" (readProcessWithExitCode file args ""))
  where
    status (ExitSuccess, output, _) = Right output
    status _ = Left (failure "asterisk_command_failed")
