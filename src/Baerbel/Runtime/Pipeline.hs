module Baerbel.Runtime.Pipeline
  ( Pipeline, PipelineRules(..), TurnState(..), TurnAction(..), stepFrame
  , preparePipeline, pipelineSession
  ) where

import Baerbel.Core.Chat
    ( ChatState(chatText),
      ChunkRules(ChunkRules),
      emptyChat,
      chatEvent,
      splitEvents )
import Baerbel.Core.Config ( Settings, setting, rule )
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( decodeBytes )
import Baerbel.Core.Pipeline
import Baerbel.Runtime.Concurrent
    ( raceWorkers, awaitReady, paceFrame )
import Baerbel.Runtime.HTTP ( StreamSend, newStreamSender )
import Baerbel.Runtime.Live ( Media(..) )
import Baerbel.Runtime.Logging ( Report, reportOwnerEvent )
import Baerbel.Runtime.Server ( socketMedia )
import Baerbel.Runtime.Speech
    ( Resampler,
      withResampler,
      resample,
      resetResampler,
      splitFrames,
      transcriptionConfig,
      speechRequest )
import Baerbel.Runtime.VAD ( VAD, withVAD, resetVAD, isSpeech )
import Baerbel.Runtime.WebSocket ( withSecureSocket )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race, withAsync, wait )
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
      isFullTBQueue,
      newTBQueueIO,
      readTBQueue,
      writeTBQueue,
      stateTVar,
      TBQueue )
import Control.Monad ( void, unless )
import Data.Aeson
    ( encode, object, Value(String, Object), KeyValue((.=)) )
import Data.Either ( fromRight )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Data.Text ( Text )
import GHC.Clock ( getMonotonicTimeNSec )
import Network.Socket ( Socket )
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Baerbel.Runtime.Effects as Effects
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS

data Pipeline = Pipeline
  { pipelineSettings :: Settings, pipelineRules :: PipelineRules, pipelineSend :: StreamSend
  , pipelineGreeting :: BS.ByteString, pipelineTimeout :: Int, pipelineSilence :: Int
  , pipelineMaximum :: Int, pipelineTokens :: Int, pipelineMessageBytes :: Int
  }

preparePipeline :: Settings -> IO (Result Pipeline)
preparePipeline settings = case pipelineConfiguration settings of
  Left err -> pure (Left err)
  Right (rules, duration, silence, maximumFrames, tokens, limit) -> do
    sender <- newStreamSender
    let initial = Pipeline settings rules sender BS.empty duration silence maximumFrames tokens limit
    greeting <- buildGreeting initial
    pure (Right initial {pipelineGreeting = fromRight BS.empty greeting})

pipelineSession :: Pipeline -> Report -> Text -> Socket -> IO (Result ())
pipelineSession pipeline report identifier socket = Effects.continue start configured
  where
    configured = rule (pipelineSettings pipeline) "runtime" "socket"
    rules = pipelineRules pipeline
    start limits = do
      let transport = socketMedia limits socket (const (pure ())) (reportOwnerEvent report identifier)
      incoming <- newTBQueueIO (fromIntegral (pipelineInputFrames rules))
      ready <- newTVarIO (BS.null (pipelineGreeting pipeline))
      started <- newTVarIO False
      let key = setting (pipelineSettings pipeline) "OPENAI_API_KEY"
          connect = withSecureSocket (pipelineMessageBytes pipeline) key (pipelineHost rules) (pipelinePort rules) (pipelinePath rules)
          call = connect (startPipeline pipeline transport incoming ready started)
      raceWorkers [pumpInput transport incoming ready, call,
        awaitReady "transcription_setup_timeout" (pipelineTimeout pipeline) started,
        pipelineOpening pipeline transport ready]

pumpInput :: Media -> TBQueue BS.ByteString -> TVar Bool -> IO (Result ())
pumpInput transport queue ready = do
  received <- receiveAudio transport
  case received of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right ())
    Right (Just bytes) -> do
      accepted <- atomically (queueInput queue ready bytes)
      let continue | accepted = pumpInput transport queue ready
                   | otherwise = pure (Left (failure "pipeline_input_overflow"))
      continue

queueInput :: TBQueue BS.ByteString -> TVar Bool -> BS.ByteString -> STM Bool
queueInput queue ready bytes = do
  accept <- readTVar ready
  full <- isFullTBQueue queue
  let enqueue
        | not accept = pure True
        | full = pure False
        | otherwise = writeTBQueue queue bytes >> pure True
  enqueue

startPipeline :: Pipeline -> Media -> TBQueue BS.ByteString -> TVar Bool -> TVar Bool -> WS.Connection -> IO (Result ())
startPipeline pipeline transport queue ready started conn = do
  let setup = sendSTT pipeline conn (transcriptionConfig (setting (pipelineSettings pipeline) "OPENAI_STT_MODEL")) >>= await
      await = Effects.continue (const (awaitUpdated conn))
  connected <- Effects.within (failure "transcription_setup_timeout") (pipelineTimeout pipeline) setup
  Effects.continue (const run) connected
  where
    run = do
      atomically (writeTVar started True)
      atomically (readTVar ready >>= check)
      runPipeline pipeline transport queue conn

pipelineOpening :: Pipeline -> Media -> TVar Bool -> IO (Result ())
pipelineOpening pipeline transport ready = do
  result <- playBytes rules transport (pipelineGreeting pipeline)
  Effects.continue (const finished) result
  where
    rules = pipelineRules pipeline
    finished = do
      unless (BS.null (pipelineGreeting pipeline)) (threadDelay (pipelineEchoMicros rules))
      atomically (writeTVar ready True)
      atomically retry

awaitUpdated :: WS.Connection -> IO (Result ())
awaitUpdated conn = do
  received <- receiveSTT conn
  case received of
    Left err -> pure (Left err)
    Right (Object fields) | KM.lookup "type" fields == Just (String "session.updated") -> pure (Right ())
    Right _ -> awaitUpdated conn

runPipeline :: Pipeline -> Media -> TBQueue BS.ByteString -> WS.Connection -> IO (Result ())
runPipeline pipeline transport queue conn = withVAD (pipelineVadMode rules) $ \vad ->
  withResampler (pipelineInputRate rules) (pipelineProviderRate rules) $ \converter -> do
    state <- newTVarIO (Listening [] 0)
    history <- newIORef []
    let input = consumeInput pipeline queue conn vad converter state BS.empty
        output = receiveTurns pipeline transport conn state history
        guarded = race output (transcriptWatchdog pipeline state)
    result <- race input guarded
    pure (either id (either id id) result)
  where
    rules = pipelineRules pipeline

consumeInput :: Pipeline -> TBQueue BS.ByteString -> WS.Connection -> VAD -> Resampler -> TVar TurnState -> BS.ByteString -> IO (Result ())
consumeInput pipeline queue conn vad converter state pending = do
  bytes <- atomically (readTBQueue queue)
  let combined = pending <> bytes
      (frames, rest) = splitFrames (pipelineFrameBytes (pipelineRules pipeline)) combined
  result <- Effects.runAll (acceptFrame pipeline conn vad converter state) frames
  Effects.continue (const (consumeInput pipeline queue conn vad converter state rest)) result

acceptFrame :: Pipeline -> WS.Connection -> VAD -> Resampler -> TVar TurnState -> BS.ByteString -> IO (Result ())
acceptFrame pipeline conn vad converter state frame = do
  now <- Effects.monotonicSeconds
  current <- readTVarIO state
  voiced <- classifyFrame (pipelineRules pipeline) vad current now frame
  actions <- atomically (transitionFrame pipeline state now voiced frame)
  Effects.continue (Effects.runAll (performTurnAction pipeline conn converter)) actions

classifyFrame :: PipelineRules -> VAD -> TurnState -> Double -> BS.ByteString -> IO (Result Bool)
classifyFrame rules vad state now frame = case state of
  Listening _ _ -> classify
  Recording _ _ -> classify
  Echo deadline | now >= deadline -> resetVAD vad (pipelineVadMode rules) >>= Effects.continue (const classify)
  _ -> pure (Right False)
  where
    classify = isSpeech vad (pipelineInputRate rules) frame

transitionFrame :: Pipeline -> TVar TurnState -> Double -> Result Bool -> BS.ByteString -> STM (Result [TurnAction])
transitionFrame pipeline state now voiced frame = case voiced of
  Left err -> pure (Left err)
  Right speech -> do
    old <- readTVar state
    let (next, actions) = stepFrame (pipelineRules pipeline) (pipelineSilence pipeline) (pipelineMaximum pipeline) now speech frame old
    writeTVar state next
    pure (Right actions)

performTurnAction :: Pipeline -> WS.Connection -> Resampler -> TurnAction -> IO (Result ())
performTurnAction pipeline conn converter (Begin bytes) = do
  reset <- resetResampler converter
  Effects.continue (const (performTurnAction pipeline conn converter (Append bytes))) reset
performTurnAction pipeline conn converter (Append bytes) =
  resample converter bytes False >>= Effects.continue (sendPCM pipeline conn)
performTurnAction pipeline conn converter Commit = do
  final <- resample converter BS.empty True
  sent <- Effects.continue (sendPCM pipeline conn) final
  Effects.continue (const (sendSTT pipeline conn (object ["type" .= ("input_audio_buffer.commit" :: Text)]))) sent

sendPCM :: Pipeline -> WS.Connection -> BS.ByteString -> IO (Result ())
sendPCM _ _ bytes | BS.null bytes = pure (Right ())
sendPCM pipeline conn bytes = sendSTT pipeline conn command
  where
    encoded = TE.decodeUtf8 (B64.encode bytes)
    command = object ["type" .= ("input_audio_buffer.append" :: Text), "audio" .= encoded]

sendSTT :: Pipeline -> WS.Connection -> Value -> IO (Result ())
sendSTT pipeline conn value = do
  let send = Right <$> WS.sendTextData conn (encode value)
  Effects.within (failure "transcription_send_timeout") (pipelineTimeout pipeline) send

receiveSTT :: WS.Connection -> IO (Result Value)
receiveSTT conn = do
  bytes <- WS.receiveData conn
  pure (decodeBytes "invalid_transcription_event" bytes >>= validateSTT)

receiveTurns :: Pipeline -> Media -> WS.Connection -> TVar TurnState -> IORef [Value] -> IO (Result ())
receiveTurns pipeline transport conn state history = do
  event <- receiveSTT conn
  accepted <- Effects.continue (atomically . acceptTranscript state) event
  result <- Effects.continue (maybe (pure (Right ())) (respondTurn pipeline transport state history)) accepted
  Effects.continue (const (receiveTurns pipeline transport conn state history)) result

acceptTranscript :: TVar TurnState -> Value -> STM (Result (Maybe Text))
acceptTranscript state event = stateTVar state advance
  where
    advance current = case transcriptStep event current of
      Left err -> (Left err, current)
      Right (next, text) -> (Right text, next)

respondTurn :: Pipeline -> Media -> TVar TurnState -> IORef [Value] -> Text -> IO (Result ())
respondTurn pipeline transport state history text = do
  observeEvent transport (object ["type" .= ("session.input_transcript.delta" :: Text), "delta" .= text])
  result <- Effects.within (failure "pipeline_response_timeout") (pipelineResponseMicros (pipelineRules pipeline))
    (respond pipeline transport history text)
  now <- Effects.monotonicSeconds
  let deadline = now + fromIntegral (pipelineEchoMicros (pipelineRules pipeline)) / 1000000
  atomically (writeTVar state (Echo deadline))
  pure result

transcriptWatchdog :: Pipeline -> TVar TurnState -> IO (Result ())
transcriptWatchdog pipeline state = do
  now <- Effects.monotonicSeconds
  current <- readTVarIO state
  case current of
    Committed started _ | (now - started) * 1000000 > fromIntegral (pipelineTimeout pipeline) ->
      pure (Left (failure "transcription_timeout"))
    _ -> threadDelay (pipelineWatchdogMicros (pipelineRules pipeline)) >> transcriptWatchdog pipeline state

respond :: Pipeline -> Media -> IORef [Value] -> Text -> IO (Result ())
respond _ _ _ text | T.null (T.strip text) = pure (Right ())
respond pipeline transport history text = do
  queue <- newTBQueueIO (fromIntegral (pipelinePrefetchFrames (pipelineRules pipeline)))
  let enqueue value = atomically (writeTBQueue queue value) >> pure (Right ())
      produce = streamReply pipeline transport history text (speak pipeline enqueue) >>= finish queue
      play = withResampler (pipelineProviderRate rules) (pipelineInputRate rules) $ \converter ->
        playQueue rules transport converter queue BS.empty
  withAsync produce $ \worker -> do
    let failed = wait worker >>= Effects.continue (const (atomically retry))
    result <- race failed play
    pure (either id id result)
  where
    rules = pipelineRules pipeline
    finish queue result = atomically (writeTBQueue queue (Nothing <$ result)) >> pure result

streamReply :: Pipeline -> Media -> IORef [Value] -> Text -> (Text -> IO (Result ())) -> IO (Result ())
streamReply pipeline transport history text speakText = do
  previous <- readIORef history
  let user = object ["role" .= ("user" :: Text), "content" .= text]
  parsed <- newIORef (BS.empty, emptyChat)
  case responseRequest (pipelineSettings pipeline) (pipelineRules pipeline) (pipelineTimeout pipeline) (pipelineTokens pipeline) (previous ++ [user]) of
    Left err -> pure (Left err)
    Right request -> do
      result <- pipelineSend pipeline request (consumeChat pipeline parsed speakText)
      (_, complete) <- readIORef parsed
      observeEvent transport (object ["type" .= ("session.output_transcript.delta" :: Text), "delta" .= chatText complete])
      let limit = pipelineHistoryMessages (pipelineRules pipeline)
          updated = finishChat limit (previous ++ [user]) complete result
      Effects.continue (\next -> writeIORef history next >> pure (Right ())) updated

consumeChat :: Pipeline -> IORef (BS.ByteString, ChatState) -> (Text -> IO (Result ())) -> BS.ByteString -> IO (Result ())
consumeChat pipeline parsed speakText bytes = do
  (pending, state) <- readIORef parsed
  case splitEvents (pending <> bytes) of
    Left err -> pure (Left err)
    Right (events, rest) -> do
      result <- Effects.foldResults (advanceChat chunks speakText) state events
      Effects.continue (\next -> writeIORef parsed (rest, next) >> pure (Right ())) result
  where
    rules = pipelineRules pipeline
    chunks = ChunkRules (pipelineChunkChars rules) (pipelineAbbreviations rules)

advanceChat :: ChunkRules -> (Text -> IO (Result ())) -> ChatState -> Value -> IO (Result ChatState)
advanceChat chunks speakText current event = case chatEvent chunks event current of
  Left err -> pure (Left err)
  Right (next, pieces) -> do
    spoken <- Effects.runAll speakText pieces
    pure (next <$ spoken)

speak :: Pipeline -> (Result (Maybe BS.ByteString) -> IO (Result ())) -> Text -> IO (Result ())
speak pipeline enqueue text = pipelineSend pipeline request consume
  where
    settings = pipelineSettings pipeline
    body = speechRequest (setting settings "OPENAI_TTS_MODEL") (setting settings "OPENAI_TTS_VOICE")
      (setting settings "OPENAI_TTS_INSTRUCTIONS") text
    rules = pipelineRules pipeline
    request = providerRequest settings (pipelineTimeout pipeline) (pipelineSpeechUrl rules) body (pipelineSpeechBytes rules)
    chunkBytes = pipelineFrameBytes rules * pipelineProviderRate rules `div` pipelineInputRate rules
    consume bytes = Effects.runAll (enqueue . Right . Just) (sizedChunks chunkBytes bytes)

playQueue :: PipelineRules -> Media -> Resampler -> TBQueue (Result (Maybe BS.ByteString)) -> BS.ByteString -> IO (Result ())
playQueue rules transport converter queue pending = do
  next <- atomically (readTBQueue queue)
  case next of
    Left err -> pure (Left err)
    Right Nothing -> finalAudio rules transport converter pending
    Right (Just bytes) -> do
      converted <- resample converter bytes False
      result <- Effects.continue (playBuffered rules transport pending) converted
      Effects.continue (playQueue rules transport converter queue) result

playBuffered :: PipelineRules -> Media -> BS.ByteString -> BS.ByteString -> IO (Result BS.ByteString)
playBuffered rules transport pending pcm = do
  let (frames, rest) = splitFrames (pipelineFrameBytes rules) (pending <> pcm)
  played <- Effects.runAll (playFrame rules transport) frames
  pure (rest <$ played)

playFrame :: PipelineRules -> Media -> BS.ByteString -> IO (Result ())
playFrame rules transport frame = do
  started <- getMonotonicTimeNSec
  result <- sendAudio transport frame
  paceFrame (pipelineFrameMicros rules) started
  pure (void result)

finalAudio :: PipelineRules -> Media -> Resampler -> BS.ByteString -> IO (Result ())
finalAudio rules transport converter pending = do
  final <- resample converter BS.empty True
  Effects.continue (playBytes rules transport . padFrames rules . (pending <>)) final

playBytes :: PipelineRules -> Media -> BS.ByteString -> IO (Result ())
playBytes rules transport bytes = fmap void (playBuffered rules transport BS.empty (padFrames rules bytes))

buildGreeting :: Pipeline -> IO (Result BS.ByteString)
buildGreeting pipeline
  | T.null (T.strip text) = pure (Right BS.empty)
  | otherwise = withResampler (pipelineProviderRate rules) (pipelineInputRate rules) $ \converter -> do
      collected <- newIORef BS.empty
      generated <- Effects.within (failure "greeting_timeout") (pipelineTimeout pipeline)
        (speak pipeline (collectGreeting rules converter collected) text)
      final <- resample converter BS.empty True
      before <- readIORef collected
      pure (generated >> final >>= validateGreeting rules before)
  where
    rules = pipelineRules pipeline
    text = setting (pipelineSettings pipeline) "PHONE_ASSISTANT_GREETING"

collectGreeting :: PipelineRules -> Resampler -> IORef BS.ByteString -> Result (Maybe BS.ByteString) -> IO (Result ())
collectGreeting _ _ _ (Left err) = pure (Left err)
collectGreeting _ _ _ (Right Nothing) = pure (Right ())
collectGreeting rules converter collected (Right (Just bytes)) = do
  converted <- resample converter bytes False
  current <- readIORef collected
  case converted >>= validateGreeting rules current of
    Left err -> pure (Left err)
    Right updated -> writeIORef collected updated >> pure (Right ())
