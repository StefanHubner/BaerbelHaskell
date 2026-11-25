module Baerbel.Core.Store where

import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Core.JSON
    ( argumentOptions, decodeUniqueIntegers, encodeBytes, jsonOptions )
import Baerbel.Core.Profile
    ( ProfileState(..), validKey, validValue )
import Baerbel.Core.ProviderConfig ( ProfileRules(..) )
import Data.Aeson ( FromJSON(..), genericParseJSON )
import Data.Char ( isPrint, isAscii, isDigit )
import Data.Maybe ( isNothing )
import Data.Text ( Text )
import Data.Time
    ( UTCTime, defaultTimeLocale, parseTimeM, toGregorian, utctDay )
import GHC.Generics ( Generic )
import System.FilePath
    ( takeDirectory, isAbsolute, splitDirectories )
import qualified Data.ByteString as Bytes ( ByteString, length )
import qualified Data.Map.Strict as Map ( null, size, toList, Map )
import qualified Data.Text as Text
    ( drop, length, strip, take, unpack )

data StoreRules = StoreRules {storeMaximumBytes :: Int, storeLockMicros :: Int, storePollMicros :: Int} deriving (Generic)
instance FromJSON StoreRules where parseJSON = genericParseJSON (jsonOptions 5)

data Stored = Stored {storedVersion :: Int, storedValues :: Map.Map Text Text, storedUpdatedAt :: Maybe Text} deriving (Generic)
instance FromJSON Stored where parseJSON = genericParseJSON (argumentOptions 6)

validatePath :: FilePath -> Result ()
validatePath path
  | not (isAbsolute path) || ".." `elem` splitDirectories path || not (all isPrint path) = Left (failure "profile_invalid_path")
  | otherwise = Right ()

parents :: FilePath -> [FilePath]
parents path
  | parent == path = [path]
  | otherwise = path : parents parent
  where parent = takeDirectory path

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

prepareSave :: StoreRules -> (ProfileState -> Result (ProfileState, a)) -> ProfileState -> Result (Maybe Bytes.ByteString, a)
prepareSave rules operation previous = do
  (next, result) <- operation previous
  bytes <- saveContent rules previous next
  Right (bytes, result)

saveContent :: StoreRules -> ProfileState -> ProfileState -> Result (Maybe Bytes.ByteString)
saveContent rules previous next
  | previous == next = Right Nothing
  | Bytes.length bytes > storeMaximumBytes rules = Left (failure "profile_storage_limit_reached")
  | otherwise = Right (Just bytes)
  where bytes = encodeBytes next <> "\n"

storageFailure :: Failure -> Failure
storageFailure issue = issue {failureCode = code}
  where
    code
      | failureUncertain issue = "profile_update_outcome_unknown_check_profile_before_retry"
      | otherwise = "profile_storage_unavailable"
