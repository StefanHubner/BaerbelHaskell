{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Profile where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON
    ( argumentOptions, decodeValue, jsonOptions )
import Baerbel.Core.Provider ( combine )
import Baerbel.Core.ProviderConfig ( ProfileRules(..) )
import Data.Aeson
    ( genericParseJSON,
      genericToJSON,
      FromJSON(parseJSON),
      Value(Object),
      ToJSON(toJSON) )
import Data.Char
    ( isDigit, isAsciiLower, GeneralCategory(..), generalCategory )
import Data.Map.Strict ( Map )
import Data.Maybe ( isNothing, isJust )
import Data.Text ( Text )
import Data.Time ( UTCTime, defaultTimeLocale, formatTime )
import GHC.Generics ( Generic )
import qualified Data.Map.Strict as Map
    ( delete, empty, filterWithKey, lookup, member, size, insert )
import qualified Data.Text as Text
    ( all, length, null, splitOn, strip, uncons, pack )

data ProfileState = ProfileState
  { profileVersion :: Int, profileValues :: Map Text Text, profileUpdatedAt :: Maybe Text }
  deriving (Eq, Show, Generic)
instance FromJSON ProfileState where parseJSON = genericParseJSON (jsonOptions 7)
instance ToJSON ProfileState where toJSON = genericToJSON (jsonOptions 7)

data Command = ReadProfile (Maybe Text) | WriteProfile Text (Maybe Text)

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

parseCommand :: ProfileRules -> Text -> Value -> Result Command
parseCommand rules "profile_get" arguments = do
  args <- decodeValue "profile_invalid_arguments" arguments
  key <- traverse (validateKey rules) (getKey args)
  Right (ReadProfile key)
parseCommand rules "profile_set" arguments = do
  args <- decodeValue "profile_invalid_arguments" arguments
  key <- validateKey rules (setKey args)
  value <- validatedValue rules (setValue args)
  Right (WriteProfile key (Just value))
parseCommand rules "profile_forget" arguments = do
  args <- decodeValue "profile_invalid_arguments" arguments
  key <- validateKey rules (forgetKey args)
  Right (WriteProfile key Nothing)
parseCommand _ _ _ = Left (failure "profile_unknown_tool")

validatedValue :: ProfileRules -> Text -> Result Text
validatedValue rules value
  | validValue rules value = Right (Text.strip value)
  | otherwise = Left (failure "profile_invalid_value")

transition :: ProfileRules -> UTCTime -> Command -> ProfileState -> Result (ProfileState, Value)
transition rules _ (ReadProfile key) state = Right (state, snapshot rules state key)
transition rules now (WriteProfile key value) state = change rules now state key value

change :: ProfileRules -> UTCTime -> ProfileState -> Text -> Maybe Text -> Result (ProfileState, Value)
change rules now state key value
  | isJust value && not (Map.member key values) && Map.size values >= profileEntryLimit rules = Left (failure "profile_entry_limit_reached")
  | otherwise = Right (updated, result)
  where
    values = profileValues state
    changed = Map.lookup key values /= value
    timestamp = Text.pack (formatTime defaultTimeLocale "%FT%TZ" now)
    entries = maybe (Map.delete key values) (\item -> Map.insert key item values) value
    updated
      | changed = state {profileValues = entries, profileUpdatedAt = Just timestamp}
      | otherwise = state
    operation = maybe "forget" (const "set") value
    detail = Change operation key value changed
    result = combine [snapshot rules updated Nothing, toJSON detail]
