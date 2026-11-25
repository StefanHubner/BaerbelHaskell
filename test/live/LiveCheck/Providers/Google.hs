module LiveCheck.Providers.Google
  ( googleRead, googleToken, gmailRead, calendarRead, contactsRead, directoryRead ) where

import Baerbel.Core.Config (Settings, rule, setting)
import Baerbel.Core.Error (Result)
import Baerbel.Core.JSON (decodeValue)
import Baerbel.Core.Provider (combine)
import Baerbel.Core.Services (AccountSpec(..))
import Baerbel.Core.TimeZones (timeZones)
import Baerbel.Runtime.Calendar (calendar)
import Baerbel.Runtime.Contacts (contacts)
import Baerbel.Runtime.Gmail (gmailSession)
import Baerbel.Runtime.HTTP (Send, authenticated, requestJSON)
import Baerbel.Runtime.OAuth (loadCredentials, tokenSource)
import Baerbel.Runtime.Provider (Execute)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson (Value(..), object, (.=))
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime, utctDay, defaultTimeLocale, formatTime)
import LiveCheck.Check (metered)
import LiveCheck.Providers.Common
import qualified Baerbel.Core.Directory as Directory
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- Credentials stay in the production OAuth boundary; only counters leave this scope.
googleRead :: Settings -> Send -> AccountSpec -> (Send -> IO (Result Value)) -> IO (Result Value)
googleRead settings sender spec action = runExceptT $ do
  (counted, requests) <- lift (metered sender)
  source <- ExceptT (googleToken settings counted spec)
  auth <- ExceptT (verifyToken source requests)
  result <- ExceptT (action (authenticated source sender))
  pure (combine [result, auth])

googleToken :: Settings -> Send -> AccountSpec -> IO (Result (IO (Result Text)))
googleToken settings sender (AccountSpec _ key scopes) = runExceptT $ do
  rules <- except (rule settings "oauth" "refresh")
  required <- except (Set.fromList <$> rule settings "oauth" scopes)
  allowed <- except (Set.fromList <$> rule settings "oauth" "allowed")
  let path = Text.unpack (setting settings key)
  credentials <- ExceptT (loadCredentials rules required allowed path)
  lift (tokenSource sender rules required allowed credentials)

verifyToken :: IO (Result Text) -> IO Int -> IO (Result Value)
verifyToken source requests = runExceptT $ do
  (_, elapsed) <- ExceptT (timed (void <$> source))
  before <- lift requests
  void (ExceptT source)
  after <- lift requests
  except (ensure "live_oauth_cache_failed" (before == 1 && after == before))
  pure (object ["oauth_requests" .= after, "oauth_seconds" .= elapsed, "oauth_cache_verified" .= True])

gmailRead :: Settings -> Send -> IO (Result Value)
gmailRead settings sender = runExceptT $ do
  rules <- except (rule settings "providers" "gmail")
  (counted, requests) <- lift (metered sender)
  execute <- lift (gmailSession rules counted noMutation getCurrentTime)
  profile <- ExceptT (profileCache execute requests)
  metadata <- ExceptT (gmailMetadata execute)
  total <- lift requests
  pure (combine [profile, metadata, object ["status" .= ("ok" :: Text), "requests" .= total]])

profileCache :: Execute -> IO Int -> IO (Result Value)
profileCache execute requests = runExceptT $ do
  (profile, cold) <- ExceptT (timed (tool execute "gmail_profile" (object [])))
  before <- lift requests
  (cached, warm) <- ExceptT (timed (tool execute "gmail_profile" (object [])))
  after <- lift requests
  except (ensure "live_gmail_profile_cache_failed" (before == 1 && after == before && cached == profile))
  pure (object ["profile_cache_verified" .= True, "cold_profile_seconds" .= cold, "warm_profile_seconds" .= warm])

gmailMetadata :: Execute -> IO (Result Value)
gmailMetadata execute = runExceptT $ do
  let args = object ["query" .= ("newer_than:30d" :: Text), "limit" .= (1 :: Int), "page_token" .= Null]
  (found, seconds) <- ExceptT (timed (tool execute "gmail_search" args))
  count <- except (member "result_count" found :: Result Int)
  pure (object ["metadata_messages" .= count, "search_seconds" .= seconds])

calendarRead :: Settings -> Send -> IO (Result Value)
calendarRead settings sender = runExceptT $ do
  rules <- except (rule settings "providers" "calendar")
  now <- lift getCurrentTime
  let day = Text.pack (formatTime defaultTimeLocale "%F" (utctDay now))
      args = object ["start_date" .= day, "end_date" .= day, "query" .= ("" :: Text), "calendar" .= ("primary" :: Text)]
      execute = calendar rules sender noMutation noReceipt timeZones
  result <- ExceptT (tool execute "calendar_search" args)
  complete <- except (member "coverage_complete" result)
  count <- except (member "event_count" result :: Result Int)
  checked <- except (member "calendars_checked" result :: Result Int)
  except (ensure "live_calendar_coverage_incomplete" (complete && checked == 1))
  pure (object ["status" .= ("ok" :: Text), "event_count" .= count, "calendars_checked" .= checked,
    "coverage_complete" .= complete])

contactsRead :: Settings -> Send -> IO (Result Value)
contactsRead settings sender = runExceptT $ do
  rules <- except (rule settings "providers" "contacts")
  (counted, requests) <- lift (metered sender)
  execute <- lift (contacts rules counted)
  (result, cold, warm) <- ExceptT (warmedSearch execute requests)
  count <- except (member "result_count" result :: Result Int)
  total <- lift requests
  pure (object ["status" .= ("ok" :: Text), "result_count" .= count, "requests" .= total,
    "warmup_cache_verified" .= True, "cold_seconds" .= cold, "warm_seconds" .= warm])

warmedSearch :: Execute -> IO Int -> IO (Result (Value, Double, Double))
warmedSearch execute requests = runExceptT $ do
  let args = object ["query" .= ("Baerbel Live Check" :: Text), "limit" .= (1 :: Int)]
  (_, cold) <- ExceptT (timed (tool execute "contacts_search" args))
  before <- lift requests
  (result, warm) <- ExceptT (timed (tool execute "contacts_search" args))
  after <- lift requests
  except (ensure "live_contacts_warmup_cache_failed" (before == 2 && after == 3))
  pure (result, cold, warm)

directoryRead :: Settings -> Send -> IO (Result Value)
directoryRead settings sender = runExceptT $ do
  rules <- except (rule settings "incoming_contacts" "directory")
  let request = Directory.directoryRequest rules Nothing
  page <- ExceptT (requestJSON sender request (decodeValue "live_directory_invalid_response"))
  (indexed, next) <- except (Directory.advanceDirectory rules 0 Set.empty Map.empty page)
  let people = length (fromMaybe [] (Directory.pageConnections page))
  pure (object ["status" .= ("ok" :: Text), "first_page_contacts" .= people,
    "indexed_numbers" .= Map.size indexed, "directory_complete" .= isNothing next])
