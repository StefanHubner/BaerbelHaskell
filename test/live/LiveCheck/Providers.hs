module LiveCheck.Providers (providerChecks) where

import Baerbel.Core.Config (Settings, setting, booleanSetting)
import Baerbel.Core.Error (Result)
import Baerbel.Core.Services (AccountSpec(..))
import Baerbel.Runtime.HTTP (Send)
import Data.Aeson (Value)
import Data.Text (Text)
import LiveCheck.Check (CheckResult, check, skip)
import LiveCheck.Providers.Google
import LiveCheck.Providers.Public
import qualified Data.Text as Text

providerChecks :: Settings -> Send -> IO [CheckResult]
providerChecks settings sender = sequence
  [ account "gmail" "PHONE_ASSISTANT_GMAIL_TOKEN_FILE" gmailRead
  , account "calendar" "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE" calendarRead
  , account "contacts" "PHONE_ASSISTANT_CONTACTS_TOKEN_FILE" contactsRead
  , directoryCheck settings sender
  , configured settings "maps" "GOOGLE_MAPS_API_KEY" (mapsRead settings sender)
  , enabled settings "windguru" "PHONE_ASSISTANT_WINDGURU" (windguruRead settings sender)
  ]
  where account = googleCheck settings sender

googleCheck :: Settings -> Send -> Text -> Text -> (Settings -> Send -> IO (Result Value)) -> IO CheckResult
googleCheck settings sender name key readProvider = configured settings name key action
  where
    spec = AccountSpec name key name
    action = googleRead settings sender spec (readProvider settings)

directoryCheck :: Settings -> Send -> IO CheckResult
directoryCheck settings sender = configured settings "contacts_directory_first_page" key action
  where
    key = "PHONE_ASSISTANT_CONTACTS_TOKEN_FILE"
    spec = AccountSpec "contacts" key "contacts"
    action = googleRead settings sender spec (directoryRead settings)

configured :: Settings -> Text -> Text -> IO (Result Value) -> IO CheckResult
configured settings name key action
  | Text.null (setting settings key) = skip name "not_configured"
  | otherwise = check name action

enabled :: Settings -> Text -> Text -> IO (Result Value) -> IO CheckResult
enabled settings name key action = case booleanSetting settings key of
  Right True -> check name action
  Right False -> skip name "disabled"
  Left issue -> check name (pure (Left issue))
