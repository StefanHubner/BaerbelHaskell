{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Live
  ( LiveConfig(..), Media(..), AudioDelivery(..), LiveControls(..), ToolLimits(..), ToolCall(..), ToolBatch(..)
  , ToolState, emptyToolState, stepTools, rememberOutput, cachedOutput
  , runLiveSession, audioCommand, toolOutputCommand, decodeAudio, abandonUncommitted, recoverable
  ) where

import Baerbel.AudioMetrics (AudioDelivery(..), AudioMetrics, AudioNotice, emptyMetrics, recordInputFrame, recordQueueDepth, recordDelivery, noticeValue, summaryValue)
import Baerbel.WebSocket (withSecureSocket)
import Baerbel.Concurrent (raceWorkers, awaitReady, hasElapsed)
import Baerbel.Background (BackgroundRules(..), Progress(..), progressStep)
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.Speech (Resampler, resample, resetResampler, splitFrames, withResampler, audiblePCM)
import Baerbel.Recording (Timeline, emptyTimeline, addFrame, resetTimeline, sentSamples, transcriptBounds)
import Data.Scientific (toRealFloat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Baerbel.Protocol (audioCommand, toolOutputCommand, decodeAudio, startCommand, continueCommand, appendCommand, announcementComplete)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (finally, onException)
import Control.Monad (foldM, unless, when, void)
import Data.Maybe (fromMaybe, isJust)
import Data.Aeson (Value(..), decodeStrict', encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.WebSockets as WS
import System.Timeout (timeout)

data ToolLimits = ToolLimits
  { maxPendingResponses :: Int, maxResponseCalls :: Int
  , maxToolCalls :: Int, maxToolText :: Int
  } deriving (Eq, Show)

data LiveConfig = LiveConfig
  { liveApiKey :: Text, liveHost :: String, livePort :: Int, livePath :: String
  , liveSession :: Value, liveGreeting :: Maybe Value
  , liveSetupMicros :: Int, liveWriteMicros :: Int
  , liveFrameBytes :: Int, liveFrameMicros :: Int
  , liveInputRate :: Int, liveProviderRate :: Int
  , liveQueueFrames :: Int, liveStartupFrames :: Int, liveToolLimits :: ToolLimits
  , liveToolMicros :: Int, liveVoiceMicros :: Int, liveDirectMicros :: Int
  , liveMutationErrors :: Map.Map Text Text, liveBackground :: BackgroundRules
  , liveMessageBytes :: Int, liveErrorFields :: Map.Map Text (Map.Map Text Value)
  } deriving (Eq)

data Media = Media
  { receiveAudio :: IO (Result (Maybe BS.ByteString))
  , sendAudio :: BS.ByteString -> IO (Result AudioDelivery)
  , finishCall :: IO (Result ())
  , observeEvent :: Value -> IO ()
  , muteInput :: IO Bool
  , beforeTool :: (Value -> IO (Result ())) -> Text -> IO (Result ())
  , installControls :: LiveControls -> IO ()
  , observeInputAudio :: BS.ByteString -> IO ()
  , afterTool :: Text -> Value -> IO ()
  , mediaPaused :: IO Bool
  , mediaEpoch :: IO Int, progressSuppressed :: IO Bool
  }

data LiveControls = LiveControls
  { playAnnouncement :: LiveConfig -> Text -> Int -> Bool -> IO (Result Bool)
  , hasPendingTools :: Text -> IO Bool
  , stopSession :: IO ()
  }

data ToolCall = ToolCall
  { toolCallId :: Text, toolName :: Text, toolArguments :: Text
  } deriving (Eq, Show)

data ToolBatch = ToolBatch
  { delegationId :: Text, responseId :: Text, batchCalls :: [ToolCall]
  } deriving (Eq, Show)

data ToolState = ToolState
  { batches :: Map.Map (Text, Text) ToolBatch
  , activeResponses :: Map.Map Text Text
  , knownCalls :: Map.Map Text ToolCall
  , outputs :: Map.Map Text Value
  , finished :: Set.Set (Text, Text), committed :: Map.Map (Text, Text) ToolBatch
  , pendingDelegations :: Set.Set Text, finishedOrder :: Seq.Seq (Text, Text)
  } deriving (Eq, Show)

emptyToolState :: ToolState
emptyToolState = ToolState Map.empty Map.empty Map.empty Map.empty Set.empty Map.empty Set.empty Seq.empty

field :: Text -> Value -> Maybe Value
field key (Object fields) = KM.lookup (Key.fromText key) fields
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of
  Just (String text) -> Just text
  _ -> Nothing

requiredText :: Int -> Text -> Value -> Result Text
requiredText limit key value = case textField key value of
  Just text | not (T.null text) && T.length text <= limit -> Right text
  _ -> Left (failure "invalid_live_tool_event")

stepTools :: ToolLimits -> Value -> ToolState -> Result (ToolState, [ToolBatch])
stepTools limits envelope state = case field "event" envelope of
  Just event -> handleToolEvent limits envelope event state
  Nothing -> Right (state, [])

handleToolEvent :: ToolLimits -> Value -> Value -> ToolState -> Result (ToolState, [ToolBatch])
handleToolEvent limits envelope event state
  | kind `notElem` kinds = Right (state, [])
  | otherwise = do
      delegation <- requiredText 200 "delegation_id" envelope
      let response = fromMaybe Null (field "response" event)
          current = textField "id" response <|> textField "response_id" event <|> Map.lookup delegation (activeResponses state)
      routeToolEvent limits delegation current kind response event state
  where
    kind = fromMaybe "" (textField "type" event)
    kinds = ["response.created", "response.output_item.done", "response.completed", "response.failed", "response.incomplete"]

routeToolEvent :: ToolLimits -> Text -> Maybe Text -> Text -> Value -> Value -> ToolState -> Result (ToolState, [ToolBatch])
routeToolEvent _ _ Nothing _ _ _ _ = Left (failure "missing_live_response_id")
routeToolEvent limits delegation (Just response) kind result event state
  | Set.member key (finished state) = Right (state, [])
  | kind == "response.created" = createBatch limits delegation response state
  | kind == "response.output_item.done" = addCall limits key event state
  | otherwise = finishBatch limits key kind result state
  where
    key = (delegation, response)

createBatch :: ToolLimits -> Text -> Text -> ToolState -> Result (ToolState, [ToolBatch])
createBatch limits delegation response state
  | T.null response || T.length response > 200 = Left (failure "invalid_live_response_id")
  | Map.notMember key (batches state) && Map.size (batches state) + Map.size (committed state) >= maxPendingResponses limits = Left (failure "live_pending_limit")
  | otherwise = Right (state { batches = Map.insertWith keep key batch (batches state),
                              activeResponses = Map.insert delegation response (activeResponses state),
                              pendingDelegations = Set.insert delegation (pendingDelegations state) }, [])
  where
    key = (delegation, response)
    batch = ToolBatch delegation response []
    keep _ old = old

addCall :: ToolLimits -> (Text, Text) -> Value -> ToolState -> Result (ToolState, [ToolBatch])
addCall limits key event state = case field "item" event of
  Just item | textField "type" item == Just "function_call" -> do
    ident <- requiredText 200 "call_id" item
    name <- requiredText 80 "name" item
    args <- requiredText (maxToolText limits) "arguments" item
    insertCall limits key (ToolCall ident name args) state
  _ -> Right (state, [])

insertCall :: ToolLimits -> (Text, Text) -> ToolCall -> ToolState -> Result (ToolState, [ToolBatch])
insertCall limits key call state
  | Just previous <- Map.lookup ident (knownCalls state), previous /= call = Left (failure "live_tool_id_reused")
  | Map.notMember ident (knownCalls state) && Map.size (knownCalls state) >= maxToolCalls limits = Left (failure "live_tool_limit")
  | otherwise = case Map.lookup key (batches state) of
      Nothing -> Left (failure "live_tool_without_response")
      Just batch -> appendCall limits key call batch state
  where
    ident = toolCallId call

appendCall :: ToolLimits -> (Text, Text) -> ToolCall -> ToolBatch -> ToolState -> Result (ToolState, [ToolBatch])
appendCall limits key call batch state
  | call `elem` batchCalls batch = Right (state, [])
  | length (batchCalls batch) >= maxResponseCalls limits = Left (failure "live_response_tool_limit")
  | otherwise = Right (state { batches = Map.insert key updated (batches state),
                              knownCalls = Map.insert (toolCallId call) call (knownCalls state) }, [])
  where
    updated = batch { batchCalls = batchCalls batch ++ [call] }

finishBatch :: ToolLimits -> (Text, Text) -> Text -> Value -> ToolState -> Result (ToolState, [ToolBatch])
finishBatch limits key kind result state = case Map.lookup key (batches state) of
  Nothing -> Right (state, [])
  Just batch -> Right (updated batch, [batch | complete && not (null (batchCalls batch))])
  where
    complete = kind == "response.completed" && textField "status" result `elem` [Nothing, Just "completed"]
    retired = retireResponses (maxToolCalls limits) [key] state {batches = Map.delete key (batches state),
                      activeResponses = Map.delete (fst key) (activeResponses state)}
    updated batch
      | complete && not (null (batchCalls batch)) = retired {committed = Map.insert key batch (committed state)}
      | otherwise = abandonCalls (batchCalls batch) cleared
      where cleared = retired {pendingDelegations = Set.delete (fst key) (pendingDelegations state)}

retireResponses :: Int -> [(Text, Text)] -> ToolState -> ToolState
retireResponses limit keys state = state {finished = Set.fromList (toList bounded), finishedOrder = bounded}
  where
    fresh = filter (`Set.notMember` finished state) keys
    appended = finishedOrder state Seq.>< Seq.fromList fresh
    excess = max 0 (Seq.length appended - limit)
    bounded = Seq.drop excess appended

rememberOutput :: Text -> Value -> ToolState -> ToolState
rememberOutput ident result state = state { outputs = Map.insert ident result (outputs state) }

cachedOutput :: Text -> ToolState -> Maybe Value
cachedOutput ident = Map.lookup ident . outputs

data InputFrame = InputFrame BS.ByteString Double
data PlaybackFrame = PlaybackFrame Int BS.ByteString

data Runtime = Runtime
  { connection :: WS.Connection, config :: LiveConfig, media :: Media
  , inputQueue :: TBQueue InputFrame, outputQueue :: TBQueue PlaybackFrame
  , toolQueue :: TBQueue ToolBatch, toolState :: TVar ToolState
  , ready :: TVar Bool, writeLock :: MVar ()
  , preview :: TVar Bool, interrupted :: TVar Bool, playing :: TVar Bool
  , generation :: TVar Int, stopping :: TVar Bool, commandSequence :: TVar Int
  , timeline :: TVar Timeline, previewMuted :: TVar Bool, playedSpeech :: TVar (Maybe Word64)
  , audioMetrics :: TVar AudioMetrics
  , runtimeStart :: Word64, finalStatus :: TVar (Bool, Maybe Value)
  , lastTranscript :: TVar Word64, noticeIds :: TVar (Set.Set Text), noticePending :: TVar (Maybe Text)
  }

type Execute = Text -> Value -> Text -> IO (Result Value)

runLiveSession :: LiveConfig -> Execute -> Media -> IO (Result ())
runLiveSession cfg execute transport = do
  startedAt <- getMonotonicTimeNSec
  input <- newTBQueueIO (fromIntegral (liveQueueFrames cfg))
  started <- newTVarIO False
  let connect = withSecureSocket (liveMessageBytes cfg) (liveApiKey cfg) (liveHost cfg) (livePort cfg) (livePath cfg)
      connected = connect (startSession cfg execute transport startedAt input started)
      session = raceWorkers [connected, setupWatchdog cfg started]
  result <- race (pumpAudio cfg transport input started BS.empty) session
  pure (either id id result)

startSession :: LiveConfig -> Execute -> Media -> Word64 -> TBQueue InputFrame -> TVar Bool -> WS.Connection -> IO (Result ())
startSession cfg execute transport startedAt input started conn = do
  runtime <- newRuntime cfg transport startedAt input started conn
  installControls transport (controls runtime)
  finally (beginSession runtime execute) (reportClosed runtime)

newRuntime :: LiveConfig -> Media -> Word64 -> TBQueue InputFrame -> TVar Bool -> WS.Connection -> IO Runtime
newRuntime cfg transport startedAt input started conn = do
  output <- newTBQueueIO (fromIntegral (liveQueueFrames cfg))
  queue <- newTBQueueIO (fromIntegral (maxPendingResponses (liveToolLimits cfg)))
  state <- newTVarIO emptyToolState
  lock <- newMVar ()
  previewing <- newTVarIO False
  interrupt <- newTVarIO False
  playingNow <- newTVarIO False
  epoch <- newTVarIO 0
  stop <- newTVarIO False
  sequenceNumber <- newTVarIO 0
  clock <- newTVarIO emptyTimeline
  mutedPreview <- newTVarIO False
  lastSpeech <- newTVarIO Nothing
  transcriptTime <- getMonotonicTimeNSec >>= newTVarIO
  notices <- newTVarIO Set.empty
  notice <- newTVarIO Nothing
  final <- newTVarIO (False, Nothing)
  metrics <- newTVarIO (emptyMetrics startedAt)
  pure (Runtime conn cfg transport input output queue state started lock previewing interrupt playingNow epoch stop sequenceNumber clock mutedPreview lastSpeech metrics startedAt final transcriptTime notices notice)

beginSession :: Runtime -> Execute -> IO (Result ())
beginSession runtime execute = do
  let cfg = config runtime
      start = startCommand (liveSession cfg)
  sent <- sendEvent runtime start
  case sent of
    Left err -> pure (Left err)
    Right () -> do
      result <- timeout (liveSetupMicros cfg) (awaitStarted runtime)
      maybe (pure (Left (failure "live_setup_timeout"))) (either (pure . Left) (const (runBridge runtime execute))) result

awaitStarted :: Runtime -> IO (Result ())
awaitStarted runtime = receiveEvent runtime >>= either (pure . Left) (observeStarted runtime)

observeStarted :: Runtime -> Value -> IO (Result ())
observeStarted runtime event = case textField "type" event of
  Just "session.started" -> do
    atomically (writeTVar (ready runtime) True)
    reportStarted runtime
    pure (Right ())
  Just "error" -> pure (Left (failure "live_setup_rejected"))
  Just "session.closed" -> pure (Left (failure "live_closed_before_start"))
  _ -> observeEvent (media runtime) event >> awaitStarted runtime

runBridge :: Runtime -> Execute -> IO (Result ())
runBridge runtime execute = do
  started <- getMonotonicTimeNSec
  atomically (writeTVar (audioMetrics runtime) (emptyMetrics started))
  bridgeWorkers runtime execute `finally` reportAudioSummary runtime

bridgeWorkers :: Runtime -> Execute -> IO (Result ())
bridgeWorkers runtime execute = withResampler inputRate providerRate $ \up ->
  withResampler providerRate inputRate $ \down -> do
    mediaGeneration <- mediaEpoch (media runtime)
    previewGeneration <- readTVarIO (generation runtime)
    let epochs = (mediaGeneration, previewGeneration)
        workers = [sendInput runtime up False epochs, receiveOutput runtime down BS.empty 0,
                   playOutput runtime, runTools runtime execute, reportProgress runtime (Progress False 0), waitStopped runtime]
    raceWorkers workers
  where
    inputRate = liveInputRate (config runtime)
    providerRate = liveProviderRate (config runtime)

pumpAudio :: LiveConfig -> Media -> TBQueue InputFrame -> TVar Bool -> BS.ByteString -> IO (Result ())
pumpAudio cfg transport queue started pending = do
  received <- receiveAudio transport
  case received of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right ())
    Right (Just bytes) -> do
      stamp <- realToFrac <$> getPOSIXTime
      let combined = pending <> bytes
          (frames, rest) = timedInput cfg stamp combined
      accepted <- atomically (enqueueInput cfg queue started frames)
      let continue | accepted = pumpAudio cfg transport queue started rest
                   | otherwise = pure (Left (failure "live_input_overflow"))
      continue

timedInput :: LiveConfig -> Double -> BS.ByteString -> ([InputFrame], BS.ByteString)
timedInput cfg stamp bytes = (zipWith timed [0 :: Int ..] frames, rest)
  where
    (frames, rest) = splitFrames (liveFrameBytes cfg) bytes
    byteRate = fromIntegral (liveInputRate cfg * 2)
    firstAt = stamp - fromIntegral (BS.length bytes) / byteRate
    frameTime = fromIntegral (liveFrameMicros cfg) / 1000000
    timed index frame = InputFrame frame (firstAt + fromIntegral index * frameTime)

enqueueInput :: LiveConfig -> TBQueue InputFrame -> TVar Bool -> [InputFrame] -> STM Bool
enqueueInput _ _ _ [] = pure True
enqueueInput cfg queue started (frame : frames) = do
  live <- readTVar started
  count <- lengthTBQueue queue
  unless (live || count < fromIntegral (liveStartupFrames cfg)) (void (readTBQueue queue))
  full <- isFullTBQueue queue
  let enqueue | full = pure False
              | otherwise = writeTBQueue queue frame >> enqueueInput cfg queue started frames
  enqueue

sendInput :: Runtime -> Resampler -> Bool -> (Int, Int) -> IO (Result ())
sendInput runtime converter greeted epochs = do
  before <- getMonotonicTimeNSec
  nextEpochs <- resetInputEpoch runtime converter epochs
  either (pure . Left) (sendInputAt runtime converter greeted before) nextEpochs

sendInputAt :: Runtime -> Resampler -> Bool -> Word64 -> (Int, Int) -> IO (Result ())
sendInputAt runtime converter greeted before nextEpochs = do
  next <- atomically (tryReadTBQueue (inputQueue runtime))
  muted <- muteInput (media runtime)
  paused <- mediaPaused (media runtime)
  previewSilence <- readTVarIO (previewMuted runtime)
  stamp <- realToFrac <$> getPOSIXTime
  let suppressed = muted || paused || previewSilence
      InputFrame frame recordedAt = selectInput (config runtime) suppressed stamp next
  atomically (modifyTVar' (timeline runtime) (addFrame recordedAt))
  converted <- resample converter frame False
  result <- either (pure . Left) (sendInputFrame runtime greeted) converted
  case result of
    Left err -> pure (Left err)
    Right spoken -> do
      atomically (modifyTVar' (audioMetrics runtime) recordInputFrame)
      waitFrame runtime before
      sendInput runtime converter spoken nextEpochs

selectInput :: LiveConfig -> Bool -> Double -> Maybe InputFrame -> InputFrame
selectInput cfg suppressed stamp next = InputFrame frame recordedAt
  where
    silence = BS.replicate (liveFrameBytes cfg) 0
    fallbackAt = stamp - fromIntegral (liveFrameMicros cfg) / 1000000
    InputFrame received recordedAt = fromMaybe (InputFrame silence fallbackAt) next
    frame | suppressed = silence
          | otherwise = received

sendInputFrame :: Runtime -> Bool -> BS.ByteString -> IO (Result Bool)
sendInputFrame runtime greeted pcm
  | BS.null pcm = pure (Right greeted)
  | otherwise = do
      marked <- atomically (recordSent runtime (BS.length pcm `div` 2))
      observeInputAudio (media runtime) pcm
      sent <- either (pure . Left) (const (sendEvent runtime (audioCommand pcm))) marked
      either (pure . Left) (const (sendGreeting runtime greeted)) sent

sendGreeting :: Runtime -> Bool -> IO (Result Bool)
sendGreeting runtime False = case liveGreeting (config runtime) of
  Just greeting -> fmap (fmap (const True)) (sendEvent runtime greeting)
  Nothing -> pure (Right True)
sendGreeting _ True = pure (Right True)

waitFrame :: Runtime -> Word64 -> IO ()
waitFrame runtime before = do
  now <- getMonotonicTimeNSec
  let elapsed = fromIntegral ((now - fromIntegral before) `div` 1000)
      delay = max 0 (liveFrameMicros (config runtime) - elapsed)
  threadDelay delay

receiveEvent :: Runtime -> IO (Result Value)
receiveEvent runtime = do
  bytes <- WS.receiveData (connection runtime) :: IO BS.ByteString
  pure (decodeEvent (liveMessageBytes (config runtime)) bytes)

decodeEvent :: Int -> BS.ByteString -> Result Value
decodeEvent limit bytes = case decodeStrict' bytes of
  Just event@(Object _) | BS.length bytes <= limit && isJust (textField "type" event) -> Right event
  _ -> Left (failure "invalid_live_event")

receiveOutput :: Runtime -> Resampler -> BS.ByteString -> Int -> IO (Result ())
receiveOutput runtime converter pending epoch = do
  received <- receiveEvent runtime
  case received of
    Left err -> pure (Left err)
    Right event -> do
      clock <- readTVarIO (timeline runtime)
      observeEvent (media runtime) (annotateTranscript clock event)
      current <- currentGeneration runtime
      let resetOutput | current /= epoch = resetResampler converter
                      | otherwise = pure (Right ())
          rest | current /= epoch = BS.empty
               | otherwise = pending
      reset <- resetOutput
      trackInterruption runtime event
      trackBackground runtime event
      either (pure . Left) (const (dispatchEvent runtime converter rest current event)) reset

dispatchEvent :: Runtime -> Resampler -> BS.ByteString -> Int -> Value -> IO (Result ())
dispatchEvent runtime converter pending epoch event = case textField "type" event of
  Just "session.closed" -> atomically (writeTVar (finalStatus runtime) (True, field "usage" event)) >> pure (Right ())
  Just "error" -> do
    observeEvent (media runtime) (object ["type" .= ("phone.provider_error" :: Text), "code" .= ("provider_error" :: Text)])
    recovered <- recoverHandoff runtime event
    either (pure . Left) (const (receiveOutput runtime converter pending epoch)) recovered
  Just "session.output_audio.delta" -> queueOutput runtime converter pending epoch event
  Just "response.event" -> do
    accepted <- atomically (updateTools runtime event)
    either (pure . Left) (const (receiveOutput runtime converter pending epoch)) accepted
  _ -> receiveOutput runtime converter pending epoch

queueOutput :: Runtime -> Resampler -> BS.ByteString -> Int -> Value -> IO (Result ())
queueOutput runtime converter pending epoch event = do
  suppressed <- readTVarIO (preview runtime)
  paused <- mediaPaused (media runtime)
  let convert | suppressed || paused = pure (Right BS.empty)
              | otherwise = either (pure . Left) (\bytes -> resample converter bytes False) (decodeAudio event)
  converted <- convert
  case converted of
    Left err -> pure (Left err)
    Right bytes -> do
      let (frames, rest) = splitFrames (liveFrameBytes (config runtime)) (pending <> bytes)
          tagged = map (PlaybackFrame epoch) frames
      accepted <- atomically (enqueuePlayback runtime tagged)
      let continue | accepted = receiveOutput runtime converter rest epoch
                   | otherwise = pure (Left (failure "live_output_overflow"))
      continue

enqueueOutput :: TBQueue a -> [a] -> STM Bool
enqueueOutput _ [] = pure True
enqueueOutput queue (value : values) = do
  full <- isFullTBQueue queue
  let enqueue | full = pure False
              | otherwise = writeTBQueue queue value >> enqueueOutput queue values
  enqueue

updateTools :: Runtime -> Value -> STM (Result ())
updateTools runtime event = do
  state <- readTVar (toolState runtime)
  case stepTools (liveToolLimits (config runtime)) event state of
    Left err -> pure (Left err)
    Right (updated, work) -> do
      accepted <- enqueueOutput (toolQueue runtime) work
      writeTVar (toolState runtime) updated
      let result | accepted = Right ()
                 | otherwise = Left (failure "live_tool_queue_overflow")
      pure result

playOutput :: Runtime -> IO (Result ())
playOutput runtime = do
  PlaybackFrame epoch frame <- atomically $ do
    next <- readTBQueue (outputQueue runtime)
    writeTVar (playing runtime) True
    pure next
  before <- getMonotonicTimeNSec
  current <- currentGeneration runtime
  paused <- mediaPaused (media runtime)
  let send | current /= epoch || paused = pure (Right Suppressed)
           | otherwise = sendAudio (media runtime) frame
  sent <- send `onException` atomically (writeTVar (playing runtime) False)
  case sent of
    Left problem -> atomically (writeTVar (playing runtime) False) >> pure (Left problem)
    Right delivery -> finishPlayback runtime before frame delivery

finishPlayback :: Runtime -> Word64 -> BS.ByteString -> AudioDelivery -> IO (Result ())
finishPlayback runtime before frame delivery = do
  now <- getMonotonicTimeNSec
  let audible = audiblePCM frame
  notices <- atomically (recordPlayed runtime now audible delivery)
  mapM_ (observeEvent (media runtime) . noticeValue) notices
  unless (delivery == Suppressed) (waitFrame runtime before)
  playOutput runtime

recordPlayed :: Runtime -> Word64 -> Bool -> AudioDelivery -> STM [AudioNotice]
recordPlayed runtime now audible delivery = do
  writeTVar (playing runtime) False
  metrics <- readTVar (audioMetrics runtime)
  let (updated, notices) = recordDelivery now delivery audible metrics
      speechEnd = now + fromIntegral (liveFrameMicros (config runtime)) * 1000
  writeTVar (audioMetrics runtime) updated
  when (delivery == Delivered && audible) (writeTVar (playedSpeech runtime) (Just speechEnd))
  pure notices

enqueuePlayback :: Runtime -> [PlaybackFrame] -> STM Bool
enqueuePlayback runtime frames = do
  accepted <- enqueueOutput (outputQueue runtime) frames
  count <- lengthTBQueue (outputQueue runtime)
  let depth = fromIntegral count
  modifyTVar' (audioMetrics runtime) (recordQueueDepth depth)
  pure accepted

reportAudioSummary :: Runtime -> IO ()
reportAudioSummary runtime = do
  metrics <- readTVarIO (audioMetrics runtime)
  let frameMicros = liveFrameMicros (config runtime)
      event = summaryValue frameMicros metrics
  observeEvent (media runtime) event

runTools :: Runtime -> Execute -> IO (Result ())
runTools runtime execute = do
  batch <- atomically (readTBQueue (toolQueue runtime))
  result <- foldM (executeCall runtime execute) (Right ()) (batchCalls batch)
  either (pure . Left) (const (continueTools runtime execute batch)) result

continueTools :: Runtime -> Execute -> ToolBatch -> IO (Result ())
continueTools runtime execute batch = do
  stopped <- readTVarIO (stopping runtime)
  let continue | stopped = atomically retry
               | otherwise = submit
  continue
  where
    submit :: IO (Result ())
    submit = do
      sent <- sendToolEvent runtime continueCommand
      atomically (modifyTVar' (toolState runtime) (\state -> state {committed = Map.delete (delegationId batch, responseId batch) (committed state)}))
      either (pure . Left) (const (runTools runtime execute)) sent

executeCall :: Runtime -> Execute -> Result () -> ToolCall -> IO (Result ())
executeCall _ _ (Left err) _ = pure (Left err)
executeCall runtime execute (Right ()) call = do
  state <- readTVarIO (toolState runtime)
  result <- maybe (invokeTool runtime execute call) (pure . Right) (cachedOutput (toolCallId call) state)
  let output = either (failureOutput (config runtime) (toolName call)) id result
  atomically (modifyTVar' (toolState runtime) (rememberOutput (toolCallId call) output))
  stopped <- readTVarIO (stopping runtime)
  let submit | stopped = atomically retry
             | otherwise = submitOutput runtime call output
  submit

submitOutput :: Runtime -> ToolCall -> Value -> IO (Result ())
submitOutput runtime call output = do
  let event = toolOutputCommand (toolCallId call) output
  sent <- sendToolEvent runtime event
  either (const (pure ())) (const (afterTool (media runtime) (toolName call) output)) sent
  pure sent

invokeTool :: Runtime -> Execute -> ToolCall -> IO (Result Value)
invokeTool runtime execute call = case decodeStrict' (TE.encodeUtf8 (toolArguments call)) of
  Just arguments@(Object _) | advertised -> do
    allowed <- beforeTool (media runtime) (sendEvent runtime) (toolName call)
    either (pure . Left) (const (boundedTool runtime execute call arguments)) allowed
  Just (Object _) -> pure (Left (failure "unknown_tool"))
  _ -> pure (Left (failure "invalid_arguments"))
  where
    advertised = toolName call `elem` advertisedNames (liveSession (config runtime))

boundedTool :: Runtime -> Execute -> ToolCall -> Value -> IO (Result Value)
boundedTool runtime execute call arguments = do
  started <- getMonotonicTimeNSec
  result <- timeout duration (execute name arguments (toolCallId call))
  ended <- getMonotonicTimeNSec
  let checked = maybe timedOut (>>= validateOutput) result
      output = either (failureOutput cfg name) id checked
      elapsed = fromIntegral (ended - started) / (1000000 :: Double)
  observeEvent (media runtime) (object ["type" .= ("phone.tool" :: Text), "tool" .= name,
    "tool_call_id" .= toolCallId call, "status" .= textField "status" output, "elapsed_ms" .= elapsed])
  pure checked
  where
    cfg = config runtime
    name = toolName call
    mutation = Map.lookup name (liveMutationErrors cfg)
    duration | name == "direct_call" = liveDirectMicros cfg
             | name == "voice_test" = liveVoiceMicros cfg
             | otherwise = liveToolMicros cfg
    timedOut = Left (maybe (failure "tool_failed") (`Failure` True) mutation)
    validateOutput value
      | validOutput (maxToolText (liveToolLimits cfg)) value = Right value
      | otherwise = Left (Failure "invalid_tool_result" (isJust mutation))

validOutput :: Int -> Value -> Bool
validOutput limit value@(Object fields) = KM.lookup "status" fields `elem` map (Just . String) ["ok", "error", "unknown"]
  && T.length (TE.decodeUtf8 (BL.toStrict (encode value))) <= limit
validOutput _ _ = False

advertisedNames :: Value -> [Text]
advertisedNames session = case field "delegation" session >>= field "responses" >>= field "tools" of
  Just (Array tools) -> foldr (\tool names -> maybe names (: names) (textField "name" tool)) [] tools
  _ -> []

failureOutput :: LiveConfig -> Text -> Failure -> Value
failureOutput cfg name err = object (base ++ fields)
  where
    status | failureUncertain err = "unknown" :: Text
           | otherwise = "error"
    base = ["status" .= status, "error" .= failureCode err]
    applicable = Map.elems (Map.filterWithKey (\prefix _ -> prefix `T.isPrefixOf` name) (liveErrorFields cfg))
    fields = [(Key.fromText key, value) | values <- applicable, (key, value) <- Map.toList values]

sendEvent :: Runtime -> Value -> IO (Result ())
sendEvent runtime event = withMVar (writeLock runtime) $ \() -> do
  let encoded = encode event
      operation = WS.sendTextData (connection runtime) encoded
  result <- timeout (liveWriteMicros (config runtime)) operation
  pure $ maybe (Left (failure "live_write_timeout")) Right result

controls :: Runtime -> LiveControls
controls runtime = LiveControls (runPreview runtime) (pendingTools runtime) stop
  where
    stop = atomically (writeTVar (stopping runtime) True)

pendingTools :: Runtime -> Text -> IO Bool
pendingTools runtime ownId = do
  state <- readTVarIO (toolState runtime)
  pure (toolsPending ownId state)

toolsPending :: Text -> ToolState -> Bool
toolsPending ownId state = not (Map.null (batches state)) || any pending (Map.keys (knownCalls state))
  where pending ident = ident /= ownId && Map.notMember ident (outputs state)

waitStopped :: Runtime -> IO (Result ())
waitStopped runtime = atomically $ do
  stopped <- readTVar (stopping runtime)
  check stopped
  pure (Right ())

trackInterruption :: Runtime -> Value -> IO ()
trackInterruption runtime event = atomically $ do
  active <- readTVar (preview runtime)
  let input = textField "type" event == Just "session.input_transcript.delta"
      spoken = maybe False (not . T.null . T.strip) (textField "delta" event)
  unless (not active || not input || not spoken) (writeTVar (interrupted runtime) True)

runPreview :: Runtime -> LiveConfig -> Text -> Int -> Bool -> IO (Result Bool)
runPreview runtime cfg completion quietMicros interruptible = do
  transcript <- newTVarIO ""
  let lastAudio = playedSpeech runtime
  atomically (writeTVar lastAudio Nothing)
  atomically (beginPreview runtime >> writeTVar (previewMuted runtime) (not interruptible))
  let transport = previewMedia runtime transcript
      session = runLiveSession cfg unavailable transport
      monitor = previewComplete runtime transcript lastAudio completion quietMicros interruptible
      cleanup = atomically (endPreview runtime)
      unavailable _ _ _ = pure (Left (failure "announcement_has_no_tools"))
  outcome <- finally (race session monitor) cleanup
  pure $ case outcome of
    Left (Left err) -> Left err
    Left (Right ()) -> Left (failure "announcement_incomplete")
    Right result -> result

beginPreview :: Runtime -> STM ()
beginPreview runtime = do
  writeTVar (preview runtime) True
  writeTVar (interrupted runtime) False
  modifyTVar' (generation runtime) (+ 1)
  _ <- flushTBQueue (outputQueue runtime)
  pure ()

endPreview :: Runtime -> STM ()
endPreview runtime = do
  writeTVar (preview runtime) False
  writeTVar (previewMuted runtime) False
  modifyTVar' (generation runtime) (+ 1)
  _ <- flushTBQueue (outputQueue runtime)
  pure ()

previewMedia :: Runtime -> TVar Text -> Media
previewMedia runtime transcript = Media
  { receiveAudio = atomically retry
  , sendAudio = previewAudio runtime
  , finishCall = pure (Right ())
  , observeEvent = previewTranscript transcript
  , muteInput = pure True
  , beforeTool = \_ _ -> pure (Left (failure "announcement_has_no_tools"))
  , installControls = const (pure ())
  , observeInputAudio = const (pure ())
  , afterTool = \_ _ -> pure ()
  , mediaPaused = pure False
  , mediaEpoch = pure 0, progressSuppressed = pure True
  }

previewAudio :: Runtime -> BS.ByteString -> IO (Result AudioDelivery)
previewAudio runtime bytes = do
  epoch <- currentGeneration runtime
  let frame = PlaybackFrame epoch bytes
  accepted <- atomically (enqueuePlayback runtime [frame])
  let result | accepted = Right Deferred
             | otherwise = Left (failure "live_output_overflow")
  pure result

previewTranscript :: TVar Text -> Value -> IO ()
previewTranscript transcript event = case (textField "type" event, textField "delta" event) of
  (Just "session.output_transcript.delta", Just delta) -> atomically (modifyTVar' transcript (T.takeEnd 2000 . (<> delta)))
  _ -> pure ()

previewComplete :: Runtime -> TVar Text -> TVar (Maybe Word64) -> Text -> Int -> Bool -> IO (Result Bool)
previewComplete runtime transcript lastAudio ending quietMicros interruptible = do
  cancelled <- readTVarIO (interrupted runtime)
  readyToEnd <- previewDrained runtime transcript lastAudio ending quietMicros interruptible
  let finish
        | cancelled && interruptible = pure (Right False)
        | readyToEnd = pure (Right True)
        | otherwise = threadDelay (liveFrameMicros (config runtime)) >>
            previewComplete runtime transcript lastAudio ending quietMicros interruptible
  finish

previewDrained :: Runtime -> TVar Text -> TVar (Maybe Word64) -> Text -> Int -> Bool -> IO Bool
previewDrained runtime transcript lastAudio ending quietMicros marker = do
  now <- getMonotonicTimeNSec
  text <- readTVarIO transcript
  lastSpeech <- readTVarIO lastAudio
  empty <- atomically (isEmptyTBQueue (outputQueue runtime))
  active <- readTVarIO (playing runtime)
  let complete = announcementComplete marker ending text
      quiet = maybe False (hasElapsed quietMicros now) lastSpeech
  pure (complete && quiet && empty && not active)

sendToolEvent :: Runtime -> Value -> IO (Result ())
sendToolEvent runtime event = nextToolCommand runtime event >>= sendEvent runtime

nextToolCommand :: Runtime -> Value -> IO Value
nextToolCommand runtime event = do
  index <- atomically $ do
    modifyTVar' (commandSequence runtime) (+ 1)
    readTVar (commandSequence runtime)
  pure (numberedCommand index event)

numberedCommand :: Int -> Value -> Value
numberedCommand index (Object fields) = Object (KM.insert "event_id" (String ident) fields)
  where ident = "phone_tool_" <> T.pack (show index)
numberedCommand _ value = value

setupWatchdog :: LiveConfig -> TVar Bool -> IO (Result ())
setupWatchdog cfg = awaitReady "live_setup_timeout" (liveSetupMicros cfg)

recordSent :: Runtime -> Int -> STM (Result ())
recordSent runtime count = do
  clock <- readTVar (timeline runtime)
  case sentSamples count clock of
    Left err -> pure (Left err)
    Right updated -> writeTVar (timeline runtime) updated >> pure (Right ())

annotateTranscript :: Timeline -> Value -> Value
annotateTranscript clock event@(Object fields) = case (number "start_ms", number "end_ms") of
  (Just first, Just lastTime) -> case transcriptBounds first lastTime clock of
    Just (start, end) -> Object (KM.insert "archive_start" (Number (realToFrac start)) (KM.insert "archive_end" (Number (realToFrac end)) fields))
    Nothing -> event
  _ -> event
  where
    number key = case KM.lookup key fields of
      Just (Number value) -> Just (toRealFloat value :: Double)
      _ -> Nothing
annotateTranscript _ event = event

currentGeneration :: Runtime -> IO Int
currentGeneration runtime = (+) <$> mediaEpoch (media runtime) <*> readTVarIO (generation runtime)

resetInputEpoch :: Runtime -> Resampler -> (Int, Int) -> IO (Result (Int, Int))
resetInputEpoch runtime converter previous = do
  native <- mediaEpoch (media runtime)
  previewEpoch <- readTVarIO (generation runtime)
  let current = (native, previewEpoch)
  resetChangedInput runtime converter previous current

resetChangedInput :: Runtime -> Resampler -> (Int, Int) -> (Int, Int) -> IO (Result (Int, Int))
resetChangedInput runtime converter previous current
  | current == previous = pure (Right current)
  | otherwise = do
      result <- resetResampler converter
      let changed = fst current /= fst previous
      atomically (resetInputState runtime changed)
      pure (current <$ result)

resetInputState :: Runtime -> Bool -> STM ()
resetInputState runtime clearInput = do
  modifyTVar' (timeline runtime) resetTimeline
  when clearInput (void (flushTBQueue (inputQueue runtime)))


trackBackground :: Runtime -> Value -> IO ()
trackBackground runtime event = do
  now <- getMonotonicTimeNSec
  let greeted = textField "type" event == Just "session.instructions.appended" && textField "client_event_id" event == Just "phone_greeting"
  when greeted (observeEvent (media runtime) (object ["type" .= ("phone.greeting_accepted" :: Text)]))
  atomically $ do
    let transcript = textField "type" event `elem` map Just ["session.input_transcript.delta", "session.output_transcript.delta"]
        spoken = maybe False (not . T.null . T.strip) (textField "delta" event)
    unless (not transcript || not spoken) (writeTVar (lastTranscript runtime) now)
    pending <- readTVar (noticePending runtime)
    unless (textField "type" event /= Just "session.commentary.appended" || textField "client_event_id" event /= pending)
      (writeTVar (noticePending runtime) Nothing)

reportProgress :: Runtime -> Progress -> IO (Result ())
reportProgress runtime previous = do
  let rules = liveBackground (config runtime)
  threadDelay (backgroundPollMicros rules)
  now <- getMonotonicTimeNSec
  lastSpeech <- readTVarIO (lastTranscript runtime)
  state <- readTVarIO (toolState runtime)
  hidden <- progressSuppressed (media runtime)
  paused <- mediaPaused (media runtime)
  let suppressed = hidden || paused || any (pendingControl state) (Map.elems (knownCalls state))
      active = not (Set.null (pendingDelegations state))
      (next, messages) = progressStep rules now lastSpeech active suppressed previous
  sent <- foldM send (Right ()) messages
  let spoken = any (\message -> textField "type" message == Just "session.commentary.append") messages
  when (sent == Right () && spoken) (observeEvent (media runtime) (object ["type" .= ("phone.background_progress" :: Text),
    "pending_requests" .= Set.size (pendingDelegations state)]))
  either (pure . Left) (const (reportProgress runtime next)) sent
  where
    pendingControl state call = toolName call `elem` ["direct_call", "voice_test", "end_call"] && Map.notMember (toolCallId call) (outputs state)
    send (Left err) _ = pure (Left err)
    send (Right ()) event = sendToolEvent runtime event

recoverHandoff :: Runtime -> Value -> IO (Result ())
recoverHandoff runtime event = do
  notices <- readTVarIO (noticeIds runtime)
  let recover | recoverable notices event = recoverPending runtime
              | otherwise = pure (Left (failure "live_provider_error"))
  recover

recoverPending :: Runtime -> IO (Result ())
recoverPending runtime = do
  pending <- atomically $ do
    modifyTVar' (toolState runtime) (abandonUncommitted (maxToolCalls (liveToolLimits (config runtime))))
    readTVar (noticePending runtime)
  maybe (notifyRecovery runtime) (const (pure (Right ()))) pending

recoverable :: Set.Set Text -> Value -> Bool
recoverable notices event = case field "error" event of
  Just err -> textField "type" err == Just "invalid_request_error" && textField "code" err == Just "invalid_request_error"
    && textField "message" err == Just "Responses handoff incomplete." && field "param" err `elem` [Nothing, Just Null]
    && maybe True (`Set.notMember` notices) (textField "client_event_id" err)
  Nothing -> False

abandonUncommitted :: Int -> ToolState -> ToolState
abandonUncommitted limit state = abandonCalls abandonedCalls cleared
  where
    abandonedCalls = concatMap batchCalls (Map.elems (batches state))
    protected = Set.fromList (map delegationId (Map.elems (committed state)))
    cleared = retireResponses limit (Map.keys (batches state)) state
      {batches = Map.empty, activeResponses = Map.empty, pendingDelegations = protected}

abandonCalls :: [ToolCall] -> ToolState -> ToolState
abandonCalls calls state = foldr abandon state calls
  where
    protected = Set.fromList (map toolCallId (concatMap batchCalls (Map.elems (committed state))))
    output = object ["status" .= ("error" :: Text), "error" .= ("delegation_incomplete_not_executed" :: Text)]
    abandon call current
      | Set.member ident protected || Map.member ident (outputs current) = current
      | otherwise = rememberOutput ident output current
      where ident = toolCallId call

notifyRecovery :: Runtime -> IO (Result ())
notifyRecovery runtime = do
  let rules = liveBackground (config runtime)
  instructions <- nextToolCommand runtime (appendCommand "session.instructions.append" (backgroundRecoveryInstructions rules))
  commentary <- nextToolCommand runtime (appendCommand "session.commentary.append" (backgroundRecoveryCommentary rules))
  atomically $ do
    let ids = foldr (\value rest -> maybe rest (: rest) (textField "event_id" value)) [] [instructions, commentary]
    modifyTVar' (noticeIds runtime) (Set.union (Set.fromList ids))
    writeTVar (noticePending runtime) (textField "event_id" commentary)
  first <- sendEvent runtime instructions
  either (pure . Left) (const (sendEvent runtime commentary)) first


reportStarted :: Runtime -> IO ()
reportStarted runtime = do
  now <- getMonotonicTimeNSec
  let session = liveSession (config runtime)
      voice = field "audio" session >>= field "output" >>= textField "voice"
      backend = field "delegation" session >>= field "responses" >>= textField "model"
      elapsed = fromIntegral (now - runtimeStart runtime) / (1000000 :: Double)
  observeEvent (media runtime) (object ["type" .= ("phone.session_started" :: Text), "model" .= textField "model" session,
    "voice" .= voice, "backend_model" .= backend, "setup_ms" .= elapsed])

reportClosed :: Runtime -> IO ()
reportClosed runtime = do
  now <- getMonotonicTimeNSec
  (closed, usage) <- readTVarIO (finalStatus runtime)
  let elapsed = fromIntegral (now - runtimeStart runtime) / (1000000 :: Double)
  observeEvent (media runtime) (object ["type" .= ("phone.session_closed" :: Text), "finalized" .= closed,
    "final_usage" .= usage, "duration_ms" .= elapsed])
