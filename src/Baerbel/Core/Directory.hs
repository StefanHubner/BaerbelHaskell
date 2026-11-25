module Baerbel.Core.Directory where

import Baerbel.Core.Contacts ( Person(..), Name(..), Number(..) )
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.HTTP ( Request(..), Body(..) )
import Baerbel.Core.Incoming ( callbackNumber )
import Baerbel.Core.JSON ( jsonOptions, camelOptions )
import Baerbel.Core.Provider ( firstOr, textParam )
import Data.Aeson ( FromJSON(..), genericParseJSON )
import Data.Char ( isAlpha, isAscii, isDigit )
import Data.Map.Strict ( Map )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data DirectoryRules = DirectoryRules
  { directoryEndpoint :: Text, directoryRefreshMicros :: Int, directoryMaximumAge :: Double
  , directoryPages :: Int, directoryPageSize :: Int, directoryPhoneCount :: Int
  , directoryNameChars :: Int, directoryTokenChars :: Int, directoryReadyMicros :: Int
  , directoryRefreshTimeoutMicros :: Int, directoryRequestSeconds :: Int
  , directoryResponseBytes :: Int } deriving (Generic)
instance FromJSON DirectoryRules where parseJSON = genericParseJSON (jsonOptions 9)
data Page = Page {pageConnections :: Maybe [Person], pageNextPageToken :: Maybe Text} deriving (Generic)
instance FromJSON Page where parseJSON = genericParseJSON (camelOptions 4)
type Index = Map Text (Text, Maybe Text)
data Snapshot = Snapshot {snapshotIndex :: Index, snapshotUpdated :: Maybe Double, snapshotReady :: Bool}

greetingName :: DirectoryRules -> Text -> Maybe Text
greetingName rules value
  | Text.null text || Text.length text > directoryNameChars rules || not (Text.any isAlpha text) = Nothing
  | Text.all allowed text = Just text
  | otherwise = Nothing
  where
  text = Text.unwords (Text.words value)
  allowed char = isAlpha char || char `elem` (" '-’" :: String)

canonicalNumber :: Number -> Maybe Text
canonicalNumber item
  | any (`Text.isInfixOf` Text.toCaseFold raw) ["ext", "x", "#", ";", ","] = Nothing
  | Just canonical <- numberCanonicalForm item >>= callbackNumber "allowed" = Just canonical
  | "+" `Text.isPrefixOf` raw && Text.all allowed raw = callbackNumber "allowed" cleaned
  | otherwise = Nothing
  where
  raw = fromMaybe "" (numberValue item)
  allowed char = isAscii char && isDigit char || char `elem` ("+ ()-" :: String)
  cleaned = Text.filter (`notElem` (" ()-" :: String)) raw

indexPerson :: DirectoryRules -> Int -> Index -> (Int, Person) -> Index
indexPerson rules page previous (offset, person) = foldl' insert previous numbers
  where
  identity = fromMaybe (Text.pack (show page) <> ":" <> Text.pack (show offset)) (personResourceName person)
  firstName = firstOr Nothing (maybe [] (map Just) (personNames person))
  label = firstName >>= chooseName rules
  numbers = mapMaybe canonicalNumber (take (directoryPhoneCount rules) (fromMaybe [] (personPhoneNumbers person)))
  insert index number = Map.alter choose number index
  choose Nothing = Just (identity, label)
  choose (Just (owner, name))
    | owner == identity = Just (owner, name)
    | otherwise = Just (owner, Nothing)

chooseName :: DirectoryRules -> Name -> Maybe Text
chooseName rules name = case nameGivenName name >>= greetingName rules of
  Just value -> Just value
  Nothing -> greetingName rules fallback
  where fallback = firstOr "" (Text.words (fromMaybe "" (nameDisplayName name)))

directoryRequest :: DirectoryRules -> Maybe Text -> Request
directoryRequest rules token = Request "GET" (directoryEndpoint rules) [] parameters EmptyBody (directoryResponseBytes rules) (directoryRequestSeconds rules)
  where
    parameters = [textParam "personFields" "names,phoneNumbers", textParam "pageSize" (Text.pack (show (directoryPageSize rules))), textParam "sources" "READ_SOURCE_TYPE_CONTACT"]
      ++ maybe [] (pure . textParam "pageToken") token

advanceDirectory :: DirectoryRules -> Int -> Set.Set Text -> Index -> Page -> Result (Index, Maybe Text)
advanceDirectory rules number seen index response
  | length people > directoryPageSize rules = Left (failure "incoming_contacts_invalid_response")
  | otherwise = case pageNextPageToken response of
    Nothing -> Right (updated, Nothing)
    Just "" -> Right (updated, Nothing)
    Just token | Text.length token > directoryTokenChars rules || Set.member token seen -> Left (failure "incoming_contacts_invalid_pagination")
    Just token -> Right (updated, Just token)
  where
    people = fromMaybe [] (pageConnections response)
    updated = foldl' (indexPerson rules number) index (zip [0..] people)

snapshotName :: DirectoryRules -> Double -> Text -> Snapshot -> Maybe Text
snapshotName rules now number current = snapshotUpdated current >>= recent
  where
    recent updated
      | now - updated > directoryMaximumAge rules = Nothing
      | otherwise = Map.lookup number (snapshotIndex current) >>= snd

refreshSnapshot :: Double -> Maybe (Result Index) -> Snapshot -> Snapshot
refreshSnapshot now (Just (Right index)) _ = Snapshot index (Just now) True
refreshSnapshot _ _ previous = previous {snapshotReady = True}
