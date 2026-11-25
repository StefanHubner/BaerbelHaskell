module LiveCheck.Delivery (deliveryChecks) where

import Baerbel.Core.Config (Settings, setting)
import Baerbel.Core.Error (Failure(..), Result)
import Baerbel.Core.Services (AccountSpec(..))
import Baerbel.Runtime.Effects (within)
import Baerbel.Runtime.HTTP (Send, RawSend)
import Data.Aeson (Value)
import Data.Text (Text)
import LiveCheck.Check (CheckResult, check, skip)
import LiveCheck.Delivery.Calendar (temporaryEvent)
import LiveCheck.Delivery.Common (withGoogle)
import LiveCheck.Delivery.Email (selfEmail)
import qualified Data.Text as Text

-- Invoked only by the separately selected, explicitly approved delivery command.
deliveryChecks :: Settings -> Send -> IO [CheckResult]
deliveryChecks settings sender = sequence
  [ delivery "gmail" "PHONE_ASSISTANT_GMAIL_TOKEN_FILE" (\json _ -> selfEmail settings json)
  , delivery "calendar" "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE" (temporaryEvent settings)
  ]
  where
    delivery = runDelivery settings sender

runDelivery :: Settings -> Send -> Text -> Text -> (Send -> RawSend -> IO (Result Value)) -> IO CheckResult
runDelivery settings sender name key action
  | Text.null (setting settings key) = skip label "not_configured"
  | otherwise = check label (within deadline 120000000 authorized)
  where
    label = "delivery." <> name
    deadline = Failure "live_delivery_timeout_do_not_retry" True
    account = AccountSpec name key name
    authorized = withGoogle settings sender account action
