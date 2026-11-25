{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Live
  ( LiveConfig(..), Media(..), AudioDelivery(..), LiveControls(..), ToolLimits(..), ToolCall(..), ToolBatch(..)
  , ToolState, emptyToolState, stepTools, rememberOutput, cachedOutput
  , runLiveSession, closeSession, audioCommand, toolOutputCommand, decodeAudio, abandonUncommitted, recoverable, appendTranscript, observeStarted
  ) where

import Baerbel.Core.AudioMetrics
    ( AudioDelivery(..),
      AudioMetrics,
      AudioNotice(..),
      emptyMetrics,
      recordInputFrame,
      recordQueueDepth,
      recordDelivery,
      noticeValue,
      summaryValue )
import Baerbel.Core.Background
    ( BackgroundRules(..), Progress(..), progressStep )
import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Core.JSON ( textField, eventDelta )
import Baerbel.Core.Live
import Baerbel.Core.Protocol
    ( audioCommand,
      toolOutputCommand,
      decodeAudio,
      startCommand,
      closeCommand,
      continueCommand,
      appendCommand,
      announcementReady )
import Baerbel.Runtime.Concurrent
    ( raceWorkers, awaitReady, paceStream )
import Baerbel.Runtime.Recording
    ( Timeline, emptyTimeline, addFrame, resetTimeline, sentSamples )
import Baerbel.Runtime.Speech
    ( Resampler,
      resample,
      resetResampler,
      splitFrames,
      withResampler,
      audiblePCM )
import Baerbel.Runtime.WebSocket ( withSecureSocket, socketResult )
import Control.Applicative ( (<|>) )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Concurrent.MVar ( MVar, newMVar, withMVar )
import Control.Concurrent.STM
    ( atomically,
      retry,
      check,
      STM,
      TVar,
      newTVarIO,
      readTVar,
      readTVarIO,
      writeTVar,
      flushTBQueue,
      isEmptyTBQueue,
      isFullTBQueue,
      lengthTBQueue,
      newTBQueueIO,
      readTBQueue,
      tryReadTBQueue,
      writeTBQueue,
      modifyTVar',
      TBQueue )
import Control.Exception ( finally, onException )
import Control.Monad ( unless, when, void )
import Data.Aeson
    ( Value(..), decodeStrict', encode, object, (.=) )
import Data.Maybe ( isJust )
import Data.Text ( Text )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Word ( Word64 )
import GHC.Clock ( getMonotonicTimeNSec )
import qualified Data.ByteString as BS
import qualified Baerbel.Runtime.Effects as Effects
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS

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

data Runtime = Runtime
  { connection :: WS.Connection, config :: LiveConfig, media :: Media
  , inputQueue :: TBQueue InputFrame, outputQueue :: TBQueue PlaybackFrame
  , toolQueue :: TBQueue ToolBatch, toolState :: TVar ToolState
  , ready :: TVar Bool, startSent :: TVar Bool, writeLock :: MVar ()
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
  finally (beginSession runtime execute) (finishSession runtime)

finishSession :: Runtime -> IO ()
finishSession runtime = finishProvider runtime `finally` reportClosed runtime

finishProvider :: Runtime -> IO ()
finishProvider runtime = do
  sent <- readTVarIO (startSent runtime)
  when sent (finalizeProvider runtime)

finalizeProvider :: Runtime -> IO ()
finalizeProvider runtime = do
  started <- readTVarIO (ready runtime)
  (closed, _) <- readTVarIO (finalStatus runtime)
  result <- closeSession (config runtime) started closed (connection runtime)
  let remember event = atomically (writeTVar (finalStatus runtime) (closedStatus event))
  either (const (pure ())) (mapM_ remember) result

reportClosed :: Runtime -> IO ()
reportClosed runtime = do
  now <- getMonotonicTimeNSec
  final <- readTVarIO (finalStatus runtime)
  let event = closedEvent (runtimeStart runtime) now final
  observeEvent (media runtime) event

-- Bridge readers and writers have stopped before this bounded close exchange.
closeSession :: LiveConfig -> Bool -> Bool -> WS.Connection -> IO (Result (Maybe Value))
closeSession _ _ True _ = pure (Right Nothing)
closeSession cfg started False conn = socketResult limited
  where
    duration | started = liveCloseMicros cfg
             | otherwise = liveStartupCloseMicros cfg
    limited = Effects.within (failure "live_close_timeout") duration exchange
    command = encode closeCommand
    exchange = WS.sendTextData conn command >> finish
    finish | started = awaitFinal cfg conn
           | otherwise = pure (Right Nothing)

awaitFinal :: LiveConfig -> WS.Connection -> IO (Result (Maybe Value))
awaitFinal cfg conn = receiveMessage cfg conn >>= Effects.continue observe
  where
    observe event
      | textField "type" event == Just "session.closed" = pure (Right (Just event))
      | otherwise = awaitFinal cfg conn

newRuntime :: LiveConfig -> Media -> Word64 -> TBQueue InputFrame -> TVar Bool -> WS.Connection -> IO Runtime
newRuntime cfg transport startedAt input started conn = do
  output <- newTBQueueIO (fromIntegral (liveQueueFrames cfg))
  queue <- newTBQueueIO (fromIntegral (maxPendingResponses (liveToolLimits cfg)))
  state <- newTVarIO emptyToolState
  sent <- newTVarIO False
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
  pure (Runtime conn cfg transport input output queue state started sent lock previewing interrupt playingNow epoch stop sequenceNumber clock mutedPreview lastSpeech metrics startedAt final transcriptTime notices notice)

beginSession :: Runtime -> Execute -> IO (Result ())
beginSession runtime execute = do
  let cfg = config runtime
      start = startCommand (liveSession cfg)
  sent <- sendEvent runtime start
  case sent of
    Left err -> pure (Left err)
    Right () -> do
      atomically (writeTVar (startSent runtime) True)
      result <- Effects.within (failure "live_setup_timeout") (liveSetupMicros cfg) (awaitStarted runtime)
      Effects.continue (const (runBridge runtime execute)) result

awaitStarted :: Runtime -> IO (Result ())
awaitStarted runtime = receiveEvent runtime >>= Effects.continue dispatch
  where
    dispatch :: Value -> IO (Result ())
    dispatch event = case observeStarted event of
      Left err -> pure (Left err)
      Right False -> observeEvent (media runtime) event >> awaitStarted runtime
      Right True -> started
    started :: IO (Result ())
    started = do
      atomically (writeTVar (ready runtime) True)
      now <- getMonotonicTimeNSec
      let session = liveSession (config runtime)
          event = startedEvent session (runtimeStart runtime) now
      observeEvent (media runtime) event
      pure (Right ())

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
    deadline <- getMonotonicTimeNSec
    let epochs = (mediaGeneration, previewGeneration)
        workers = [sendInput runtime up False epochs deadline, receiveOutput runtime down BS.empty 0,
                   playOutput runtime deadline, runTools runtime execute, reportProgress runtime (Progress False 0), waitStopped runtime]
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

sendInput :: Runtime -> Resampler -> Bool -> (Int, Int) -> Word64 -> IO (Result ())
sendInput runtime converter greeted epochs deadline = do
  next <- paceStream (liveFrameMicros (config runtime)) deadline
  nextEpochs <- resetInputEpoch runtime converter epochs
  Effects.continue (sendInputAt runtime converter greeted next) nextEpochs

sendInputAt :: Runtime -> Resampler -> Bool -> Word64 -> (Int, Int) -> IO (Result ())
sendInputAt runtime converter greeted deadline nextEpochs = do
  next <- atomically (tryReadTBQueue (inputQueue runtime))
  muted <- muteInput (media runtime)
  paused <- mediaPaused (media runtime)
  previewSilence <- readTVarIO (previewMuted runtime)
  stamp <- realToFrac <$> getPOSIXTime
  let suppressed = muted || paused || previewSilence
      InputFrame frame recordedAt = selectInput (config runtime) suppressed stamp next
  atomically (modifyTVar' (timeline runtime) (addFrame recordedAt))
  converted <- resample converter frame False
  result <- Effects.continue (sendInputFrame runtime greeted) converted
  case result of
    Left err -> pure (Left err)
    Right spoken -> do
      atomically (modifyTVar' (audioMetrics runtime) recordInputFrame)
      sendInput runtime converter spoken nextEpochs deadline

sendInputFrame :: Runtime -> Bool -> BS.ByteString -> IO (Result Bool)
sendInputFrame runtime greeted pcm
  | BS.null pcm = pure (Right greeted)
  | otherwise = do
      marked <- atomically (recordSent runtime (BS.length pcm `div` 2))
      observeInputAudio (media runtime) pcm
      sent <- Effects.continue (const (sendEvent runtime (audioCommand pcm))) marked
      Effects.continue (const (sendGreeting runtime greeted)) sent

sendGreeting :: Runtime -> Bool -> IO (Result Bool)
sendGreeting runtime False = case liveGreeting (config runtime) of
  Just greeting -> fmap (fmap (const True)) (sendEvent runtime greeting)
  Nothing -> pure (Right True)
sendGreeting _ True = pure (Right True)

receiveEvent :: Runtime -> IO (Result Value)
receiveEvent runtime = receiveMessage (config runtime) (connection runtime)

receiveMessage :: LiveConfig -> WS.Connection -> IO (Result Value)
receiveMessage cfg conn = do
  bytes <- WS.receiveData conn :: IO BS.ByteString
  pure (decodeEvent (liveMessageBytes cfg) bytes)

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
      atomically (readTVar (preview runtime) >>= \active ->
        when (trackInterruption active event) (writeTVar (interrupted runtime) True))
      trackBackground runtime event
      Effects.continue (const (dispatchEvent runtime converter rest current event)) reset

dispatchEvent :: Runtime -> Resampler -> BS.ByteString -> Int -> Value -> IO (Result ())
dispatchEvent runtime converter pending epoch event = case textField "type" event of
  Just "session.closed" -> atomically (writeTVar (finalStatus runtime) (closedStatus event)) >> pure (Right ())
  Just "error" -> do
    observeEvent (media runtime) (object ["type" .= ("phone.provider_error" :: Text), "code" .= ("provider_error" :: Text)])
    recovered <- recoverHandoff runtime event
    Effects.continue (const (receiveOutput runtime converter pending epoch)) recovered
  Just "session.output_audio.delta" -> queueOutput runtime converter pending epoch event
  Just "response.event" -> do
    accepted <- atomically (updateTools runtime event)
    Effects.continue (const (receiveOutput runtime converter pending epoch)) accepted
  _ -> receiveOutput runtime converter pending epoch

queueOutput :: Runtime -> Resampler -> BS.ByteString -> Int -> Value -> IO (Result ())
queueOutput runtime converter pending epoch event = do
  suppressed <- readTVarIO (preview runtime)
  paused <- mediaPaused (media runtime)
  let convert | suppressed || paused = pure (Right BS.empty)
              | otherwise = Effects.continue (\bytes -> resample converter bytes False) (decodeAudio event)
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

playOutput :: Runtime -> Word64 -> IO (Result ())
playOutput runtime deadline = do
  PlaybackFrame epoch frame <- atomically $ do
    next <- readTBQueue (outputQueue runtime)
    writeTVar (playing runtime) True
    pure next
  readyToPlay <- playbackCurrent runtime epoch
  let send | readyToPlay = sendPlayback runtime deadline epoch frame
           | otherwise = pure (Right (deadline, Suppressed))
  sent <- send `onException` atomically (writeTVar (playing runtime) False)
  case sent of
    Left problem -> atomically (writeTVar (playing runtime) False) >> pure (Left problem)
    Right (next, delivery) -> finishPlayback runtime next frame delivery

playbackCurrent :: Runtime -> Int -> IO Bool
playbackCurrent runtime epoch = do
  current <- currentGeneration runtime
  paused <- mediaPaused (media runtime)
  pure (current == epoch && not paused)

sendPlayback :: Runtime -> Word64 -> Int -> BS.ByteString -> IO (Result (Word64, AudioDelivery))
sendPlayback runtime deadline epoch frame = do
  next <- paceStream (liveFrameMicros (config runtime)) deadline
  readyToPlay <- playbackCurrent runtime epoch
  let send | readyToPlay = sendAudio (media runtime) frame
           | otherwise = pure (Right Suppressed)
  delivered <- send
  pure ((,) next <$> delivered)

finishPlayback :: Runtime -> Word64 -> BS.ByteString -> AudioDelivery -> IO (Result ())
finishPlayback runtime deadline frame delivery = do
  now <- getMonotonicTimeNSec
  let audible = audiblePCM frame
  notices <- atomically (recordPlayed runtime now audible delivery)
  let period = fromIntegral (liveFrameMicros (config runtime)) * 1000
      next = case notices of
        AudioStarted _ : _ -> now + period
        _ -> deadline
  mapM_ (observeEvent (media runtime) . noticeValue) notices
  playOutput runtime next

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
  result <- Effects.runAll (executeCall runtime execute) (batchCalls batch)
  Effects.continue (const (continueTools runtime execute batch)) result

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
      Effects.continue (const (runTools runtime execute)) sent

executeCall :: Runtime -> Execute -> ToolCall -> IO (Result ())
executeCall runtime execute call = do
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
    Effects.continue (const (boundedTool runtime execute call arguments)) allowed
  Just (Object _) -> pure (Left (failure "unknown_tool"))
  _ -> pure (Left (failure "invalid_arguments"))
  where
    advertised = toolName call `elem` advertisedNames (liveSession (config runtime))

boundedTool :: Runtime -> Execute -> ToolCall -> Value -> IO (Result Value)
boundedTool runtime execute call arguments = do
  started <- getMonotonicTimeNSec
  result <- Effects.within timedOut duration (execute name arguments (toolCallId call))
  ended <- getMonotonicTimeNSec
  let checked = result >>= validateOutput
      output = either (failureOutput cfg name) id checked
      elapsed = fromIntegral (ended - started) / (1000000 :: Double)
  observeEvent (media runtime) (object ["type" .= ("phone.tool" :: Text), "tool" .= name,
    "tool_call_id" .= toolCallId call, "status" .= textField "status" output,
    "error" .= textField "error" output, "elapsed_ms" .= elapsed])
  pure checked
  where
    cfg = config runtime
    name = toolName call
    mutation = Map.lookup name (liveMutationErrors cfg)
    duration | name == "direct_call" = liveDirectMicros cfg
             | otherwise = liveToolMicros cfg
    timedOut = maybe (failure "tool_failed") (`Failure` True) mutation
    validateOutput value
      | validOutput (maxToolText (liveToolLimits cfg)) value = Right value
      | otherwise = Left (Failure "invalid_tool_result" (isJust mutation))

sendEvent :: Runtime -> Value -> IO (Result ())
sendEvent runtime event = withMVar (writeLock runtime) $ \() -> do
  let encoded = encode event
      operation = WS.sendTextData (connection runtime) encoded
  Effects.within (failure "live_write_timeout") (liveWriteMicros (config runtime)) (Right <$> operation)

controls :: Runtime -> LiveControls
controls runtime = LiveControls (runPreview runtime) pending stop
  where
    pending ownId = pendingTools ownId <$> readTVarIO (toolState runtime)
    stop = atomically (writeTVar (stopping runtime) True)

waitStopped :: Runtime -> IO (Result ())
waitStopped runtime = atomically $ do
  stopped <- readTVar (stopping runtime)
  check stopped
  pure (Right ())

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
  , observeEvent = mapM_ (atomically . modifyTVar' transcript) . appendTranscript
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

previewComplete :: Runtime -> TVar Text -> TVar (Maybe Word64) -> Text -> Int -> Bool -> IO (Result Bool)
previewComplete runtime transcript lastAudio ending quietMicros interruptible = do
  cancelled <- readTVarIO (interrupted runtime)
  now <- getMonotonicTimeNSec
  text <- readTVarIO transcript
  lastSpeech <- readTVarIO lastAudio
  empty <- atomically (isEmptyTBQueue (outputQueue runtime))
  active <- readTVarIO (playing runtime)
  let complete = announcementReady interruptible ending quietMicros now text lastSpeech
      readyToEnd = complete && empty && not active
      finish
        | cancelled && interruptible = pure (Right False)
        | readyToEnd = pure (Right True)
        | otherwise = threadDelay (liveFrameMicros (config runtime)) >>
            previewComplete runtime transcript lastAudio ending quietMicros interruptible
  finish

sendToolEvent :: Runtime -> Value -> IO (Result ())
sendToolEvent runtime event = nextToolCommand runtime event >>= sendEvent runtime

nextToolCommand :: Runtime -> Value -> IO Value
nextToolCommand runtime event = do
  index <- atomically $ do
    modifyTVar' (commandSequence runtime) (+ 1)
    readTVar (commandSequence runtime)
  pure (numberedCommand index event)

setupWatchdog :: LiveConfig -> TVar Bool -> IO (Result ())
setupWatchdog cfg = awaitReady "live_setup_timeout" (liveSetupMicros cfg)

recordSent :: Runtime -> Int -> STM (Result ())
recordSent runtime count = do
  clock <- readTVar (timeline runtime)
  case sentSamples count clock of
    Left err -> pure (Left err)
    Right updated -> writeTVar (timeline runtime) updated >> pure (Right ())

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
    let delta = eventDelta "session.input_transcript.delta" event <|> eventDelta "session.output_transcript.delta" event
        spoken = maybe False (not . T.null . T.strip) delta
    when spoken (writeTVar (lastTranscript runtime) now)
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
  sent <- Effects.runAll (sendToolEvent runtime) messages
  let spoken = any (\message -> textField "type" message == Just "session.commentary.append") messages
  when (sent == Right () && spoken) (observeEvent (media runtime) (object ["type" .= ("phone.background_progress" :: Text),
    "pending_requests" .= Set.size (pendingDelegations state)]))
  Effects.continue (const (reportProgress runtime next)) sent
  where
    pendingControl state call = toolName call `elem` ["direct_call", "end_call"] && Map.notMember (toolCallId call) (outputs state)

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
  Effects.continue (const (sendEvent runtime commentary)) first
