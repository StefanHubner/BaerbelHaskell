{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Contacts (contacts, candidate, Candidate(..), Phone(..), Person(..), Name(..), Number(..)) where

import Baerbel.Error (Result, failure)
import Baerbel.HTTP (Send)
import Baerbel.JSON (argumentOptions, camelOptions, jsonOptions, decodeValue)
import Baerbel.ProviderConfig (ContactsRules(..))
import Baerbel.Provider (Execute, clean, fetch, firstOr, get, textParam)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.Aeson hiding (Result)
import Data.Char (isDigit)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Query = Query {queryQuery :: Text, queryLimit :: Int} deriving (Generic)
instance FromJSON Query where parseJSON = genericParseJSON (argumentOptions 5)

data Search = Search {searchText :: Text, searchLimit :: Int}

data Name = Name {nameDisplayName :: Maybe Text, nameGivenName :: Maybe Text} deriving (Generic)
instance FromJSON Name where parseJSON = genericParseJSON (camelOptions 4)

data Number = Number
  { numberValue :: Maybe Text, numberFormattedType :: Maybe Text
  , numberType :: Maybe Text, numberCanonicalForm :: Maybe Text } deriving (Generic)
instance FromJSON Number where parseJSON = genericParseJSON (camelOptions 6)

data Person = Person {personNames :: Maybe [Name], personPhoneNumbers :: Maybe [Number], personResourceName :: Maybe Text} deriving (Generic)
instance FromJSON Person where parseJSON = genericParseJSON (camelOptions 6)
newtype Match = Match {matchPerson :: Person} deriving (Generic)
instance FromJSON Match where parseJSON = genericParseJSON (camelOptions 5)
newtype Matches = Matches {matchesResults :: Maybe [Match]} deriving (Generic)
instance FromJSON Matches where parseJSON = genericParseJSON (camelOptions 7)

data Phone = Phone
  { phoneLabel :: Text, phoneNumber :: Text, phonePhoneNumberE164 :: Maybe Text
  , phoneExtensionPresent :: Bool } deriving (Eq, Show, Generic)
instance ToJSON Phone where toJSON = genericToJSON (jsonOptions 5)

data Candidate = Candidate
  { candidateName :: Text, candidateOtherNames :: [Text], candidateNamesTruncated :: Bool
  , candidatePhoneNumbers :: [Phone], candidateMultiplePhoneNumbers :: Bool
  , candidatePhoneNumbersTruncated :: Bool } deriving (Eq, Show, Generic)
instance ToJSON Candidate where toJSON = genericToJSON (jsonOptions 9)

data SearchResult = SearchResult
  { searchStatus :: Text, searchSource :: Text, searchCandidates :: [Candidate]
  , searchResultCount :: Int, searchHasMore :: Bool, searchMultipleMatches :: Bool
  , searchSearchScope :: Text, searchUntrustedContactContent :: Bool } deriving (Generic)
instance ToJSON SearchResult where toJSON = genericToJSON (jsonOptions 6)

candidate :: ContactsRules -> Person -> Candidate
candidate rules person = Candidate label other (length names > contactsNameCount rules) phones (length numbers > 1) (length numbers > contactsPhoneCount rules)
  where
    names = fromMaybe [] (personNames person)
    numbers = fromMaybe [] (personPhoneNumbers person)
    labels = nub (filter (not . Text.null) (map display (take (contactsNameCount rules) names)))
    display = clean (contactsNameChars rules) . fromMaybe "" . nameDisplayName
    label = firstOr "" labels
    other = drop 1 labels
    phones = map (phone rules) (take (contactsPhoneCount rules) numbers)

phone :: ContactsRules -> Number -> Phone
phone rules number = Phone label value dial extension
  where
    raw = fromMaybe "" (numberValue number)
    value = clean (contactsNumberChars rules) raw
    label = clean (contactsLabelChars rules) (fromMaybe (fromMaybe "" (numberType number)) (numberFormattedType number))
    lowered = Text.toLower raw
    extension = any (`Text.isInfixOf` lowered) ["ext", "x", "#", ";", ","]
    dial = case numberCanonicalForm number of
      Just supplied | validNumber supplied && not extension -> Just supplied
      _ -> Nothing

validNumber :: Text -> Bool
validNumber value = case Text.uncons value of
  Just ('+', digits) -> Text.length digits <= 15 && Text.length digits >= 2 && Text.all isDigit digits && not ("0" `Text.isPrefixOf` digits)
  _ -> False

lookupContacts :: ContactsRules -> Send -> Text -> Int -> IO (Result Matches)
lookupContacts rules send query limit = fetch "contacts_invalid_response" operation
  where
    pageSize = Text.pack (show limit)
    params = [textParam "query" query, textParam "pageSize" pageSize,
      textParam "readMask" "names,phoneNumbers", textParam "sources" "READ_SOURCE_TYPE_CONTACT"]
    operation = get (contactsHttp rules) send (contactsEndpoint rules) params

warm :: ContactsRules -> Send -> MVar Bool -> IO (Result ())
warm rules send ready = modifyMVar ready operation
  where
    operation True = pure (True, Right ())
    operation False = do
      result <- lookupContacts rules send "" 1
      case result of
        Left issue -> pure (False, Left issue)
        Right _ -> threadDelay delay >> pure (True, Right ())
    delay = contactsWarmupSeconds rules * 1000000

contacts :: ContactsRules -> Send -> IO Execute
contacts rules send = do
  ready <- newMVar False
  pure (execute rules send ready)

execute :: ContactsRules -> Send -> MVar Bool -> Execute
execute rules send ready name arguments _
  | name /= "contacts_search" = pure (Left (failure "contacts_unknown_tool"))
  | otherwise = either (pure . Left) (search rules send ready) prepared
  where prepared = decodeValue "contacts_invalid_arguments" arguments >>= prepareSearch rules

prepareSearch :: ContactsRules -> Query -> Result Search
prepareSearch rules query
  | Text.null text || Text.length text > contactsQueryChars rules || count < 1 || count > contactsSearchLimit rules = Left (failure "contacts_invalid_arguments")
  | otherwise = Right (Search text count)
  where
    text = Text.strip (queryQuery query)
    count = queryLimit query

search :: ContactsRules -> Send -> MVar Bool -> Search -> IO (Result Value)
search rules send ready query = do
  warmed <- warm rules send ready
  either (pure . Left) (const perform) warmed
  where
    count = searchLimit query
    requested = count + 1
    perform = fmap (fmap (toJSON . summarize rules count)) (lookupContacts rules send (searchText query) requested)

summarize :: ContactsRules -> Int -> Matches -> SearchResult
summarize rules count response = SearchResult "ok" "google_contacts" found size more multiple scope True
  where
    matches = fromMaybe [] (matchesResults response)
    found = map (candidate rules . matchPerson) (take count matches)
    size = length found
    more = length matches > count
    multiple = length matches > 1
    scope = contactsSearchScope rules
