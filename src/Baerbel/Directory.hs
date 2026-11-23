module Baerbel.Directory (DirectoryRules(..), LookupCaller, greetingName, withDirectory) where

import Baerbel.Contacts (Person(..), Name(..), Number(..))
import Baerbel.Error (Result, failure)
import Baerbel.HTTP (Send, Request(..), Body(..))
import Baerbel.Incoming (callbackNumber)
import Baerbel.JSON (camelOptions, jsonOptions)
import Baerbel.Provider (fetch, firstOr, textParam)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Char (isAlpha, isAscii, isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.Timeout (timeout)

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
type LookupCaller = Text -> IO (Maybe Text)
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
  choose (Just (owner, name)) = Just (owner, if owner == identity then name else Nothing)

chooseName :: DirectoryRules -> Name -> Maybe Text
chooseName rules name = case nameGivenName name >>= greetingName rules of
  Just value -> Just value
  Nothing -> greetingName rules fallback
  where fallback = firstOr "" (Text.words (fromMaybe "" (nameDisplayName name)))

loadDirectory :: DirectoryRules -> Send -> IO (Result Index)
loadDirectory rules sender = page 0 Nothing Set.empty Map.empty
  where
  page number token seen index
    | number >= directoryPages rules = pure (Left (failure "incoming_contacts_directory_limit"))
    | otherwise = loadPage number token seen index
  loadPage number token seen index = do
    result <- fetch "incoming_contacts_invalid_response" (sender (request token))
    either (pure . Left) (advance number seen index) result
  advance number seen index response
    | length people > directoryPageSize rules = pure (Left (failure "incoming_contacts_invalid_response"))
    | otherwise = continue (number + 1) seen updated (pageNextPageToken response)
    where
    people = fromMaybe [] (pageConnections response)
    updated = foldl' (indexPerson rules number) index (zip [0..] people)
  continue _ _ index Nothing = pure (Right index)
  continue _ _ index (Just "") = pure (Right index)
  continue number seen index (Just token)
    | Text.length token > directoryTokenChars rules || Set.member token seen = pure (Left (failure "incoming_contacts_invalid_pagination"))
    | otherwise = page number (Just token) (Set.insert token seen) index
  request token = Request "GET" (directoryEndpoint rules) [] parameters EmptyBody (directoryResponseBytes rules) (directoryRequestSeconds rules)
    where parameters = [textParam "personFields" "names,phoneNumbers", textParam "pageSize" (Text.pack (show (directoryPageSize rules))), textParam "sources" "READ_SOURCE_TYPE_CONTACT"] ++ maybe [] (\value -> [textParam "pageToken" value]) token

seconds :: IO Double
seconds = fmap ((/ 1000000000) . fromIntegral) getMonotonicTimeNSec

withDirectory :: DirectoryRules -> Maybe Send -> (LookupCaller -> IO (Result a)) -> IO (Result a)
withDirectory _ Nothing action = action (const (pure Nothing))
withDirectory rules (Just sender) action = do
  state <- newTVarIO (Snapshot Map.empty Nothing False)
  withAsync (forever (refresh rules sender state >> threadDelay (directoryRefreshMicros rules))) (const (action (lookupCaller rules state)))

refresh :: DirectoryRules -> Send -> TVar Snapshot -> IO ()
refresh rules sender state = do
  result <- timeout (directoryRefreshTimeoutMicros rules) (loadDirectory rules sender)
  now <- seconds
  atomically (modifyTVar' state (update now result))
  where
  update now (Just (Right index)) _ = Snapshot index (Just now) True
  update _ _ previous = previous {snapshotReady = True}

lookupCaller :: DirectoryRules -> TVar Snapshot -> LookupCaller
lookupCaller rules state number = do
  void (timeout (directoryReadyMicros rules) (atomically (readTVar state >>= check . snapshotReady)))
  now <- seconds
  current <- readTVarIO state
  pure (snapshotUpdated current >>= recent now current)
  where
  recent now current updated
    | now - updated > directoryMaximumAge rules = Nothing
    | otherwise = Map.lookup number (snapshotIndex current) >>= snd
