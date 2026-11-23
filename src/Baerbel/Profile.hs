{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Profile (ProfileState(..), ProfileStore, profile, transition, emptyProfile, readEntry, snapshot, validKey, validValue) where

import Baerbel.Error (Result, failure)
import Baerbel.JSON (argumentOptions, decodeValue, jsonOptions)
import Baerbel.ProviderConfig (ProfileRules(..))
import Baerbel.Provider (Execute, combine)
import Data.Aeson hiding (Result)
import Data.Char (isDigit, isAsciiLower, GeneralCategory(..), generalCategory)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)

data ProfileState = ProfileState
  { profileVersion :: Int, profileValues :: Map Text Text, profileUpdatedAt :: Maybe Text }
  deriving (Eq, Show, Generic)
instance FromJSON ProfileState where parseJSON = genericParseJSON (jsonOptions 7)
instance ToJSON ProfileState where toJSON = genericToJSON (jsonOptions 7)

type ProfileStore = (ProfileState -> Result (ProfileState, Value)) -> IO (Result Value)

newtype GetArgs
  = GetArgs {getKey :: Maybe Text}
  deriving (Generic)
instance FromJSON GetArgs where parseJSON = genericParseJSON (argumentOptions 3)
data SetArgs = SetArgs {setKey :: Text, setValue :: Text} deriving (Generic)
instance FromJSON SetArgs where parseJSON = genericParseJSON (argumentOptions 3)
newtype ForgetArgs = ForgetArgs {forgetKey :: Text} deriving (Generic)
instance FromJSON ForgetArgs where parseJSON = genericParseJSON (argumentOptions 6)
data Snapshot = Snapshot
  { snapshotStatus :: Text, snapshotSource :: Text, snapshotValues :: Map Text Text
  , snapshotSnapshotComplete :: Bool, snapshotUpdatedAt :: Maybe Text
  , snapshotUntrustedProfileContent :: Bool, snapshotHomeLocationRole :: Text } deriving (Generic)
instance ToJSON Snapshot where toJSON = genericToJSON (jsonOptions 8)
data Entry = Entry {entryKey :: Text, entryFound :: Bool, entryValue :: Maybe Text} deriving (Generic)
instance ToJSON Entry where toJSON = genericToJSON (jsonOptions 5)
data Change = Change
  { changeOperation :: Text, changeKey :: Text, changeValue :: Maybe Text, changeChanged :: Bool } deriving (Generic)
instance ToJSON Change where toJSON = genericToJSON (jsonOptions 6)

emptyProfile :: ProfileState
emptyProfile = ProfileState 1 Map.empty Nothing

validKey :: ProfileRules -> Text -> Bool
validKey rules key = Text.length key <= profileKeyChars rules && initial && all part parts
  where
    parts = Text.splitOn "_" key
    initial = maybe False (isAsciiLower . fst) (Text.uncons key)
    part value = not (Text.null value) && Text.all (\char -> isAsciiLower char || isDigit char) value

validValue :: ProfileRules -> Text -> Bool
validValue rules value = not (Text.null (Text.strip value)) && Text.length value <= profileValueChars rules && Text.all printable value
  where printable char = char == ' ' || generalCategory char `notElem` [Space, LineSeparator, ParagraphSeparator, Control, Format, Surrogate, PrivateUse, NotAssigned]

validateKey :: ProfileRules -> Text -> Result Text
validateKey rules key
  | validKey rules key = Right key
  | otherwise = Left (failure "profile_invalid_key")

snapshot :: ProfileRules -> ProfileState -> Maybe Text -> Value
snapshot rules state key = combine [toJSON base, entry]
  where
    values = maybe (profileValues state) (\name -> Map.filterWithKey (\stored _ -> stored == name) (profileValues state)) key
    base = Snapshot "ok" "local_profile" values (isNothing key) (profileUpdatedAt state) True (profileHomeLocationRole rules)
    entry = maybe (Object mempty) (\name -> toJSON (Entry name (Map.member name values) (Map.lookup name values))) key

readEntry :: ProfileRules -> ProfileState -> Text -> Result Value
readEntry rules state key = do
  valid <- validateKey rules key
  Right (snapshot rules state (Just valid))

transition :: ProfileRules -> UTCTime -> Text -> Value -> ProfileState -> Result (ProfileState, Value)
transition rules now name arguments state = case name of
  "profile_get" -> do
    args <- decodeValue "profile_invalid_arguments" arguments
    key <- traverse (validateKey rules) (getKey args)
    Right (state, snapshot rules state key)
  "profile_set" -> do
    args <- decodeValue "profile_invalid_arguments" arguments
    key <- validateKey rules (setKey args)
    let value = Text.strip (setValue args)
    if not (validValue rules (setValue args))
      then Left (failure "profile_invalid_value")
      else change rules now state key (Just value)
  "profile_forget" -> do
    args <- decodeValue "profile_invalid_arguments" arguments
    key <- validateKey rules (forgetKey args)
    change rules now state key Nothing
  _ -> Left (failure "profile_unknown_tool")

change :: ProfileRules -> UTCTime -> ProfileState -> Text -> Maybe Text -> Result (ProfileState, Value)
change rules now state key value
  | isJust value && not (Map.member key values) && Map.size values >= profileEntryLimit rules = Left (failure "profile_entry_limit_reached")
  | otherwise = Right (updated, result)
  where
    values = profileValues state
    changed = Map.lookup key values /= value
    timestamp = Text.pack (formatTime defaultTimeLocale "%FT%TZ" now)
    entries = maybe (Map.delete key values) (\item -> Map.insert key item values) value
    updated = if changed then state {profileValues = entries, profileUpdatedAt = Just timestamp} else state
    operation = if isJust value then "set" else "forget"
    detail = Change operation key value changed
    result = combine [snapshot rules updated Nothing, toJSON detail]

profile :: ProfileRules -> ProfileStore -> IO UTCTime -> Execute
profile rules store clock name arguments _ = do
  now <- clock
  let operation = transition rules now name arguments
  store operation
