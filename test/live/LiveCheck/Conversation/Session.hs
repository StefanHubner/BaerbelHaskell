module LiveCheck.Conversation.Session where

import Baerbel.Core.Config (Settings(..))
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.Live (LiveConfig(..))
import Baerbel.Core.Protocol (buildSession)
import Baerbel.Core.Session (liveConfiguration)
import Baerbel.Core.Speech (audiblePCM, splitFrames)
import Baerbel.Runtime.Concurrent (paceStream)
import Baerbel.Runtime.Effects (continue, within)
import Baerbel.Runtime.Live (Media(..), LiveControls(..), AudioDelivery(..), runLiveSession)
import Baerbel.Runtime.Provider (Execute)
import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)
import Data.Maybe (fromMaybe)
import Control.Concurrent.Async (Async, AsyncCancelled, cancel, poll, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import LiveCheck.Check (ensure, emit)
import LiveCheck.Conversation.Model
import qualified Data.ByteString as Bytes
import qualified Data.Map.Strict as Map

-- The harness implements only telephone I/O; all Live orchestration stays production code.
data Harness = Harness
  { harnessTrace :: TVar Trace, harnessInput :: TQueue ByteString
  , harnessControls :: TVar (Maybe LiveControls), harnessCancelled :: TVar Bool
  , harnessClock :: TVar Word64, harnessConfig :: LiveConfig, harnessStarted :: Word64 }

data Running = Running {runningHarness :: Harness, runningTask :: Async (Result ())}

ownerConfiguration :: Settings -> UTCTime -> [Value] -> Result LiveConfig
ownerConfiguration settings now schemas = do
  session <- buildSession selected now schemas
  liveConfiguration selected session ""
  where
    selected = settings {settingsValues = Map.union overrides (settingsValues settings)}
    overrides = Map.fromList [("PHONE_ASSISTANT_PRIVATE_PIN_HASH", ""), ("PHONE_ASSISTANT_WEB_SEARCH", "false")]

withConversation :: Settings -> [Value] -> (Harness -> Execute) -> (Running -> IO (Result Value)) -> IO (Result Value)
withConversation settings schemas execute action = do
  now <- getCurrentTime
  continue (\cfg -> withConfigured cfg execute action) (ownerConfiguration settings now schemas)

withConfigured :: LiveConfig -> (Harness -> Execute) -> (Running -> IO (Result Value)) -> IO (Result Value)
withConfigured cfg = withConfiguredMedia cfg id

withConfiguredMedia :: LiveConfig -> (Media -> Media) -> (Harness -> Execute) -> (Running -> IO (Result Value)) -> IO (Result Value)
withConfiguredMedia cfg prepareMedia execute action = do
  harness <- newHarness cfg
  let transport = prepareMedia (media harness)
      session = runLiveSession cfg (observedExecute harness (execute harness)) transport
  withAsync session (runAction harness action)

newHarness :: LiveConfig -> IO Harness
newHarness cfg = do
  started <- getMonotonicTimeNSec
  Harness <$> newTVarIO emptyTrace <*> newTQueueIO <*> newTVarIO Nothing
    <*> newTVarIO False <*> newTVarIO started <*> pure cfg <*> pure started

runAction :: Harness -> (Running -> IO (Result Value)) -> Async (Result ()) -> IO (Result Value)
runAction harness action task = do
  let running = Running harness task
  ready <- await running "live_check_start_timeout" 30000000 (\_ -> seen "phone.session_started")
  result <- continue (const (action running)) ready
  closed <- finishSession running
  trace <- snapshot running
  cancelled <- readTVarIO (harnessCancelled harness)
  let reported = finishReport cancelled harness trace result closed
  diagnose harness trace reported
  pure reported

finishReport :: Bool -> Harness -> Trace -> Result Value -> Result Double -> Result Value
finishReport cancelled harness trace result closed = do
  detail <- result
  shutdown <- closed
  ensure "live_check_provider_error" (summaryProviderErrors metrics == 0)
  ensure "live_check_missing_closed_event" (summaryLocalCleanupObserved metrics)
  ensure "live_check_provider_not_finalized" (cancelled || summaryProviderFinalized metrics)
  Right (object ["conversation" .= detail, "audio" .= metrics, "shutdown_ms" .= shutdown])
  where metrics = summarize (harnessStarted harness) (liveInputRate (harnessConfig harness)) trace

media :: Harness -> Media
media harness = Media
  { receiveAudio = Right . Just <$> receiveFrame harness
  , sendAudio = delivered harness
  , finishCall = atomically (modifyTVar' (harnessTrace harness) markFinished) >> pure (Right ())
  , observeEvent = observe harness
  , muteInput = pure False
  , beforeTool = \_ _ -> pure (Right ())
  , installControls = atomically . writeTVar (harnessControls harness) . Just
  , observeInputAudio = observedInput harness
  , afterTool = \name _ -> observe harness (toolEvent "check.tool.submitted" name)
  , mediaPaused = pure False, mediaEpoch = pure 0, progressSuppressed = pure False
  }

observedInput :: Harness -> ByteString -> IO ()
observedInput harness bytes = observe harness event
  where
    count = Bytes.length bytes
    event = object ["type" .= ("check.input.audio" :: Text), "bytes" .= count]

receiveFrame :: Harness -> IO ByteString
receiveFrame harness = do
  deadline <- readTVarIO (harnessClock harness)
  next <- paceStream (liveFrameMicros cfg) deadline
  atomically $ do
    writeTVar (harnessClock harness) next
    frame <- tryReadTQueue (harnessInput harness)
    pure (fromMaybe silence frame)
  where
    cfg = harnessConfig harness
    silence = Bytes.replicate (liveFrameBytes cfg) 0

observe :: Harness -> Value -> IO ()
observe harness value = do
  now <- getMonotonicTimeNSec
  atomically (modifyTVar' (harnessTrace harness) (recordEvent now value))

delivered :: Harness -> ByteString -> IO (Result AudioDelivery)
delivered harness bytes = do
  now <- getMonotonicTimeNSec
  let count = Bytes.length bytes
      speech = audiblePCM bytes
  atomically (modifyTVar' (harnessTrace harness) (recordFrame now count speech))
  pure (Right Delivered)

observedExecute :: Harness -> Execute -> Execute
observedExecute harness execute name arguments ident = do
  observe harness (toolEvent "check.tool.started" name)
  result <- execute name arguments ident
  observe harness (toolEvent "check.tool.finished" name)
  pure result

toolEvent :: Text -> Text -> Value
toolEvent kind name = object ["type" .= kind, "tool" .= name]

snapshot :: Running -> IO Trace
snapshot = readTVarIO . harnessTrace . runningHarness

await :: Running -> Text -> Int -> (Word64 -> Trace -> Bool) -> IO (Result Trace)
await running code duration predicate = within (failure code) duration loop
  where
    loop = do
      now <- getMonotonicTimeNSec
      trace <- snapshot running
      advance now trace
    advance now trace
      | predicate now trace = pure (Right trace)
      | otherwise = poll (runningTask running) >>= pending
    pending Nothing = threadDelay 20000 >> loop
    pending (Just (Right (Left issue))) = pure (Left issue)
    pending (Just _) = pure (Left (failure "live_check_session_ended_early"))

say :: Running -> ByteString -> IO Word64
say running bytes = do
  started <- getMonotonicTimeNSec
  foldM_ (sendFrame harness) started complete
  getMonotonicTimeNSec
  where
    harness = runningHarness running
    size = liveFrameBytes (harnessConfig harness)
    (frames, rest) = splitFrames size bytes
    complete = frames ++ trailing size rest

trailing :: Int -> ByteString -> [ByteString]
trailing size bytes
  | Bytes.null bytes = []
  | otherwise = [bytes <> Bytes.replicate (size - Bytes.length bytes) 0]

sendFrame :: Harness -> Word64 -> ByteString -> IO Word64
sendFrame harness deadline bytes = do
  next <- paceStream (liveFrameMicros (harnessConfig harness)) deadline
  atomically (writeTQueue (harnessInput harness) bytes)
  pure next

controls :: Running -> IO (Result LiveControls)
controls running = maybe (Left (failure "live_check_controls_missing")) Right
  <$> readTVarIO (harnessControls (runningHarness running))

finishSession :: Running -> IO (Result Double)
finishSession running = do
  started <- getMonotonicTimeNSec
  installed <- readTVarIO (harnessControls (runningHarness running))
  mapM_ stopSession installed
  result <- within (failure "live_check_shutdown_timeout") 6000000 (Right <$> waitCatch (runningTask running))
  expected <- readTVarIO (harnessCancelled (runningHarness running))
  ended <- getMonotonicTimeNSec
  pure (milliseconds started ended <$ (result >>= closedResult expected))

closedResult :: Bool -> Either SomeException (Result ()) -> Result ()
closedResult _ (Right result) = result
closedResult True (Left issue) = case fromException issue :: Maybe AsyncCancelled of
  Just _ -> Right ()
  Nothing -> Left (failure "live_check_session_exception")
closedResult False (Left _) = Left (failure "live_check_session_exception")

cancelConversation :: Running -> IO (Result Value)
cancelConversation running = do
  alive <- poll (runningTask running)
  case alive of
    Just _ -> pure (Left (failure "live_check_cancelled_after_session_end"))
    Nothing -> cancelActive running

cancelActive :: Running -> IO (Result Value)
cancelActive running = do
  started <- getMonotonicTimeNSec
  atomically (writeTVar (harnessCancelled (runningHarness running)) True)
  result <- within (failure "live_check_cancellation_timeout") 6000000 (Right <$> cancel (runningTask running))
  ended <- getMonotonicTimeNSec
  let elapsed = milliseconds started ended
  pure (object ["cancellation_ms" .= elapsed] <$ result)

answer :: Running -> ByteString -> [Text] -> IO (Result Value)
answer running clip expected = do
  end <- say running clip
  result <- await running "live_check_answer_timeout" 35000000 (answered end expected)
  pure (toJSON . turnTiming end <$> result)

diagnose :: Harness -> Trace -> Result Value -> IO ()
diagnose _ _ (Right _) = pure ()
diagnose harness trace (Left _) = emit (object ["diagnostic" .= ("conversation_failure" :: Text), "audio" .= metrics])
  where metrics = summarize (harnessStarted harness) (liveInputRate (harnessConfig harness)) trace
