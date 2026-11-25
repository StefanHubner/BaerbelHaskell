{-# LANGUAGE ForeignFunctionInterface #-}
module Baerbel.Store (StoreRules(..), profileStore, validateProfileFile) where

import qualified Baerbel.Effects as Effects
import Baerbel.Storage (atomicWrite)
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.JSON (argumentOptions, decodeUniqueIntegers, encodeBytes, jsonOptions)
import Baerbel.Profile (ProfileState(..), ProfileStore(..), emptyProfile, validKey, validValue)
import Baerbel.ProviderConfig (ProfileRules(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (IOException, bracket, try)
import Data.Aeson (FromJSON(..), genericParseJSON)
import qualified Data.ByteString as Bytes
import Data.Bits ((.&.))
import Data.Char (isPrint, isAscii, isDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM, toGregorian, utctDay)
import Foreign.C.Types (CInt(..))
import System.FilePath (takeDirectory, isAbsolute, splitDirectories)
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
import System.Posix.IO (OpenMode(..), OpenFileFlags(..), defaultFileFlags, openFd, closeFd, fdToHandle, dup)
import GHC.Generics (Generic)
import qualified System.Posix.Types
import System.Posix.Types (Fd(..), UserID)
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

foreign import ccall unsafe "flock" fileLock :: CInt -> CInt -> IO CInt

data StoreRules = StoreRules {storeMaximumBytes :: Int, storeLockMicros :: Int, storePollMicros :: Int} deriving (Generic)
instance FromJSON StoreRules where parseJSON = genericParseJSON (jsonOptions 5)

data Stored = Stored {storedVersion :: Int, storedValues :: Map.Map Text Text, storedUpdatedAt :: Maybe Text} deriving (Generic)
instance FromJSON Stored where parseJSON = genericParseJSON (argumentOptions 6)

profileStore :: ProfileRules -> StoreRules -> FilePath -> IO (Result ProfileStore)
profileStore profileRules rules path = do
  checked <- validateProfileFile profileRules rules path
  gate <- newMVar ()
  let operation step = withMVar gate (const (locked profileRules rules path step))
      store = ProfileStore (readCurrentProfile profileRules rules path) operation
  pure (store <$ checked)

readCurrentProfile :: ProfileRules -> StoreRules -> FilePath -> IO (Result ProfileState)
readCurrentProfile profileRules rules path = do
  owner <- inspectOwner path
  Effects.continue (readProfile profileRules rules path) owner

validateProfileFile :: ProfileRules -> StoreRules -> FilePath -> IO (Result ())
validateProfileFile profileRules rules path = Effects.continue inspect (validatePath path)
  where
  inspect () = fmap (const () <$>) (readCurrentProfile profileRules rules path)

validatePath :: FilePath -> Result ()
validatePath path
  | not (isAbsolute path) || ".." `elem` splitDirectories path || not (all isPrint path) = Left (failure "profile_invalid_path")
  | otherwise = Right ()

inspectOwner :: FilePath -> IO (Result UserID)
inspectOwner path = Effects.attempt "profile_storage_unavailable" inspect
  where
    parent = takeDirectory path
    inspect = do
      ancestors <- traverse getSymbolicLinkStatus (parents parent)
      metadata <- getFileStatus parent
      profileOwner ancestors metadata <$> getEffectiveUserID

profileOwner :: [FileStatus] -> FileStatus -> UserID -> Result UserID
profileOwner ancestors metadata effective
  | safe = Right (fileOwner metadata)
  | otherwise = Left (failure "profile_unsafe_storage")
  where
    safe = not (any isSymbolicLink ancestors) && isDirectory metadata && permissions metadata == 0o700
      && (effective == 0 || effective == fileOwner metadata)

parents :: FilePath -> [FilePath]
parents path
  | parent == path = [path]
  | otherwise = path : parents parent
  where parent = takeDirectory path

permissions :: FileStatus -> System.Posix.Types.FileMode
permissions metadata = fileMode metadata .&. 0o7777

privateRegular :: UserID -> Bool -> FileStatus -> Bool
privateRegular owner unlinked metadata = isRegularFile metadata && fileOwner metadata == owner
  && permissions metadata == 0o600 && (linkCount metadata == 1 || unlinked && linkCount metadata == 0)

locked :: ProfileRules -> StoreRules -> FilePath -> (ProfileState -> Result (ProfileState, a)) -> IO (Result a)
locked profileRules rules path operation = do
  owner <- inspectOwner path
  Effects.continue protect owner
  where
  flags = defaultFileFlags {creat = Just 0o600, nofollow = True, nonBlock = True, cloexec = True}
  protect uid = Effects.attempt "profile_storage_unavailable" (bracket (openFd (path ++ ".lock") ReadWrite flags) closeFd (run uid))
  run uid descriptor = withPrivateDescriptor uid False descriptor (acquire uid descriptor)
  acquire uid descriptor = do
    obtained <- timeout (storeLockMicros rules) (awaitLock rules descriptor)
    maybe (pure (Left (failure "profile_busy_try_again"))) (const (update profileRules rules path uid operation)) obtained

withPrivateDescriptor :: UserID -> Bool -> Fd -> IO (Result a) -> IO (Result a)
withPrivateDescriptor owner unlinked descriptor action = getFdStatus descriptor >>= use
  where
    use metadata
      | privateRegular owner unlinked metadata = action
      | otherwise = pure (Left (failure "profile_unsafe_storage"))

awaitLock :: StoreRules -> Fd -> IO ()
awaitLock rules descriptor@(Fd raw) = fileLock raw 6 >>= continue
  where
    continue 0 = pure ()
    continue _ = threadDelay (storePollMicros rules) >> awaitLock rules descriptor

update :: ProfileRules -> StoreRules -> FilePath -> UserID -> (ProfileState -> Result (ProfileState, a)) -> IO (Result a)
update profileRules rules path uid operation = do
  previous <- readProfile profileRules rules path uid
  Effects.continue (persist rules path) (previous >>= apply)
  where
  apply previous = do
    (next, result) <- operation previous
    Right (previous, next, result)

readProfile :: ProfileRules -> StoreRules -> FilePath -> UserID -> IO (Result ProfileState)
readProfile profileRules rules path owner = Effects.attempt "profile_storage_unavailable" openedFile
  where
  openedFile = do
    opened <- try (openFd path ReadOnly flags)
    either (pure . missingProfile) (\descriptor -> bracket (pure descriptor) closeFd readOwned) opened
  flags = defaultFileFlags {nofollow = True, nonBlock = True, cloexec = True}
  readOwned descriptor = withPrivateDescriptor owner True descriptor (readBytes descriptor)
  readBytes descriptor = do
    copy <- dup descriptor
    let bound = storeMaximumBytes rules + 1
    raw <- bracket (fdToHandle copy) hClose (`Bytes.hGet` bound)
    pure (boundedState profileRules rules raw)

missingProfile :: IOException -> Result ProfileState
missingProfile issue
  | isDoesNotExistError issue = Right emptyProfile
  | otherwise = Left (failure "profile_storage_unavailable")

boundedState :: ProfileRules -> StoreRules -> Bytes.ByteString -> Result ProfileState
boundedState profileRules rules raw
  | Bytes.length raw > storeMaximumBytes rules = Left (failure "profile_invalid_state")
  | otherwise = decodeState profileRules raw

decodeState :: ProfileRules -> Bytes.ByteString -> Result ProfileState
decodeState rules raw = decodeUniqueIntegers "profile_invalid_state" raw >>= validateState rules

validateState :: ProfileRules -> Stored -> Result ProfileState
validateState rules stored
  | storedVersion stored /= 1 || Map.size entries > profileEntryLimit rules || not (all validEntry (Map.toList entries)) = invalid
  | not (Map.null entries) && isNothing timestamp || not validTime = invalid
  | otherwise = Right (ProfileState 1 entries timestamp)
  where
  entries = storedValues stored
  timestamp = storedUpdatedAt stored
  validTime = maybe True validTimestamp timestamp
  validEntry (key, value) = validKey rules key && validValue rules value && Text.strip value == value
  invalid = Left (failure "profile_invalid_state")

validTimestamp :: Text -> Bool
validTimestamp value = shaped && seconds < "60" && maybe False positiveYear parsed
  where
    parsed :: Maybe UTCTime
    parsed = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack value)
    seconds :: Text
    seconds = Text.take 2 (Text.drop 17 value)
    shaped :: Bool
    shaped = Text.length value == 20 && and (zipWith matches "dddd-dd-ddTdd:dd:ddZ" (Text.unpack value))
    matches :: Char -> Char -> Bool
    matches 'd' char = isAscii char && isDigit char
    matches expected char = expected == char
    positiveYear :: UTCTime -> Bool
    positiveYear time = let (year, _, _) = toGregorian (utctDay time) in year > 0

persist :: StoreRules -> FilePath -> (ProfileState, ProfileState, a) -> IO (Result a)
persist rules path (previous, next, result)
  | previous == next = pure (Right result)
  | Bytes.length bytes > storeMaximumBytes rules = pure (Left (failure "profile_storage_limit_reached"))
  | otherwise = fmap (either rejected (const (Right result))) (atomicWrite path bytes)
  where
  bytes = encodeBytes next <> "\n"
  rejected issue = Left issue {failureCode = storageFailure issue}

storageFailure :: Failure -> Text
storageFailure issue
  | failureUncertain issue = "profile_update_outcome_unknown_check_profile_before_retry"
  | otherwise = "profile_storage_unavailable"
