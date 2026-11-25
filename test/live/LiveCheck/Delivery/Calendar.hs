module LiveCheck.Delivery.Calendar (temporaryEvent, runEvent, prepareProbe) where

import Baerbel.Core.Calendar
import Baerbel.Core.Config (Settings, rule)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.HTTP (Body(..), Request(..), Response(..))
import Baerbel.Core.JSON (decodeBytes, field, textField)
import Baerbel.Core.Provider (get, combine, textParam)
import Baerbel.Core.ProviderConfig (CalendarRules(..))
import Baerbel.Core.TimeFormat (isoUtc)
import Baerbel.Core.TimeZones (timeZones)
import Baerbel.Runtime.Calendar (calendar)
import Baerbel.Runtime.Effects (attempt, within)
import Baerbel.Runtime.HTTP (Send, RawSend, requestJSON)
import Baerbel.Runtime.Ledger (Ledger, durable, lookupExisting)
import Control.Exception (finally)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime(..), getCurrentTime, addUTCTime)
import LiveCheck.Check (ensure, emit)
import LiveCheck.Delivery.Common (freshId, withReceiptLedger)
import qualified Data.Aeson.KeyMap as KeyMap

-- The unique ID is computed by the production calendar builder before submission.
data EventProbe = EventProbe
  { probeRules :: CalendarRules, probeId :: Text, probeArguments :: Value
  , probeResource :: EventResource, probePeriod :: Period }

temporaryEvent :: Settings -> Send -> RawSend -> IO (Result Value)
temporaryEvent settings sender raw = runExceptT $ do
  rules <- except (rule settings "providers" "calendar")
  now <- lift getCurrentTime
  ident <- lift freshId
  probe <- except (prepareProbe rules now ident)
  ExceptT (withReceiptLedger (runEvent probe sender raw))

prepareProbe :: CalendarRules -> UTCTime -> Text -> Result EventProbe
prepareProbe rules now ident = do
  let rounded = UTCTime (utctDay now) (fromInteger (floor (utctDayTime now)))
      start = addUTCTime 300 rounded
      end = addUTCTime 120 start
      description = "Authorised temporary Haskell live check. Run " <> ident <> ". Deleted after verification."
      args = CreateArgs "Bärbel Haskell live check" (isoUtc start) (isoUtc end) description "" [] True
  prepared <- prepareCreate rules timeZones args
  let normalized = createArguments prepared
      resource = creationResource rules ident normalized
  Right (EventProbe rules ident (toJSON normalized) resource (createPeriod prepared))

runEvent :: EventProbe -> Send -> RawSend -> Ledger -> IO (Result Value)
runEvent probe sender raw ledger = do
  submitted <- newIORef False
  cleaned <- newIORef (Right (object ["cleanup_required" .= False]))
  let observed = privateSubmission probe submitted sender
      run = createAndRead probe observed sender ledger
      cleanup = finishCleanup probe raw submitted cleaned
  result <- run `finally` cleanup
  removed <- readIORef cleaned
  pure (combine <$> sequence [removed, result])

privateSubmission :: EventProbe -> IORef Bool -> Send -> Send
privateSubmission probe submitted sender request
  | requestMethod request == "POST" && requestUrl request == eventUrl (probeRules probe) "primary" = do
      writeIORef submitted True
      sender (privateRequest request)
  | otherwise = sender request

privateRequest :: Request -> Request
privateRequest request = request {requestBody = protected (requestBody request)}
  where
    protected (JsonBody (Object fields)) = JsonBody (Object updated)
      where updated = KeyMap.insert "visibility" (String "private")
              (KeyMap.insert "transparency" (String "transparent")
              (KeyMap.insert "reminders" (object ["useDefault" .= False]) fields))
    protected body = body

createAndRead :: EventProbe -> Send -> Send -> Ledger -> IO (Result Value)
createAndRead probe submit sender ledger = runExceptT $ do
  let rules = probeRules probe
      execute = calendar rules submit (durable ledger) (lookupExisting ledger) timeZones
  result <- ExceptT (execute "calendar_create" (probeArguments probe) (probeId probe))
  except (createdResult result)
  let request = eventRequest probe "GET"
  ExceptT (requestJSON sender request (verifyEvent probe))

createdResult :: Value -> Result ()
createdResult value = case (textField "status" value, field "created" value) of
  (Just "ok", Just (Bool True)) -> Right ()
  (Just "unknown", _) -> Left (Failure "live_calendar_creation_unknown_do_not_retry" True)
  _ -> Left (failure "live_calendar_creation_failed")

verifyEvent :: EventProbe -> Value -> Result Value
verifyEvent probe value = do
  let rules = probeRules probe
      resource = probeResource probe
  _ <- creationReceipt rules timeZones (probePeriod probe) resource value
  ensure "live_calendar_not_private" (textField "visibility" value == Just "private")
  ensure "live_calendar_marker_mismatch" (textField "description" value == Just (resourceDescription resource))
  ensure "live_calendar_attendees_present" (emptyAttendees (field "attendees" value))
  Right (object ["status" .= ("ok" :: Text), "created" .= True, "read_back_verified" .= True,
    "private" .= True, "attendees" .= (0 :: Int), "event_duration_seconds" .= (120 :: Int)])

emptyAttendees :: Maybe Value -> Bool
emptyAttendees Nothing = True
emptyAttendees (Just Null) = True
emptyAttendees (Just (Array values)) = null values
emptyAttendees _ = False

eventRequest :: EventProbe -> ByteString -> Request
eventRequest probe method = request {requestMethod = method}
  where
    rules = probeRules probe
    url = eventUrl rules "primary" <> "/" <> resourceId (probeResource probe)
    parameters | method == "DELETE" = [textParam "sendUpdates" "none"]
               | otherwise = []
    request = get (calendarHttp rules) url parameters

finishCleanup :: EventProbe -> RawSend -> IORef Bool -> IORef (Result Value) -> IO ()
finishCleanup probe sender submitted output = do
  attempted <- readIORef submitted
  case attempted of
    False -> pure ()
    True -> do
      let issue = Failure "live_calendar_cleanup_timeout" True
      result <- attempt "live_calendar_cleanup_io_failed" (within issue 45000000 (removeEvent probe sender))
      writeIORef output result
      emit (cleanupReport result)

removeEvent :: EventProbe -> RawSend -> IO (Result Value)
removeEvent probe sender = runExceptT $ do
  deleted <- ExceptT (sender (eventRequest probe "DELETE"))
  let deletion = responseStatus deleted
  except (ensure "live_calendar_delete_failed" (deletion `elem` [204, 404, 410]))
  checked <- ExceptT (sender (eventRequest probe "GET"))
  state <- except (deletionState (resourceId (probeResource probe)) checked)
  pure (object ["cleanup_required" .= True, "delete_status" .= deletion,
    "verification_status" .= responseStatus checked, "event_state" .= state, "cleanup_verified" .= True])

deletionState :: Text -> Response -> Result Text
deletionState ident response
  | responseStatus response `elem` [404, 410] = Right "absent"
  | responseStatus response == 200 = do
      value <- decodeBytes "live_calendar_cleanup_invalid_response" (responseBody response)
      let cancelled = textField "id" value == Just ident && textField "status" value == Just "cancelled"
      ensure "live_calendar_cleanup_not_confirmed" cancelled
      Right "cancelled"
  | otherwise = Left (failure "live_calendar_cleanup_not_confirmed")

cleanupReport :: Result Value -> Value
cleanupReport (Right value) = object ["calendar_cleanup" .= value]
cleanupReport (Left issue) = object ["calendar_cleanup" .= object
  ["cleanup_verified" .= False, "error" .= failureCode issue, "uncertain" .= failureUncertain issue]]
