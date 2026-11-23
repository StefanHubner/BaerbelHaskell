module Baerbel.Pipeline
  ( Pipeline, PipelineRules(..), TurnState(..), TurnAction(..), stepFrame
  , preparePipeline, pipelineSession
  ) where

import Baerbel.WebSocket (withSecureSocket)
import Baerbel.Concurrent (raceWorkers, awaitReady)
import Baerbel.Chat
import Baerbel.Config (Settings, setting, rule, integerSetting, numericSetting, ruleSection)
import Baerbel.Error (Result, failure)
import Baerbel.HTTP (StreamSend, Request(..), Body(..), newStreamSender)
import Baerbel.JSON (jsonOptions, decodeBytes)
import Baerbel.Logging (Report, reportOwnerEvent)
import Baerbel.Live (Media(..))
import Baerbel.Server (socketMedia)
import Baerbel.Speech (Resampler, withResampler, resample, resetResampler, splitFrames, transcriptionConfig, speechRequest)
import Baerbel.VAD (VAD, withVAD, resetVAD, isSpeech)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, withAsync, wait)
import Control.Concurrent.STM
import Control.Monad (foldM, void)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON, object, (.=), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import qualified Network.WebSockets as WS
import System.Timeout (timeout)

data PipelineRules = PipelineRules
  { pipelineHost :: String, pipelinePort :: Int, pipelinePath :: String
  , pipelineResponsesUrl :: Text, pipelineSpeechUrl :: Text, pipelineVadMode :: Int
  , pipelineFrameBytes :: Int, pipelineFrameMicros :: Int, pipelineInputRate :: Int
  , pipelineProviderRate :: Int, pipelinePrerollFrames :: Int, pipelineStartFrames :: Int
  , pipelineEchoMicros :: Int, pipelinePrefetchFrames :: Int, pipelineInputFrames :: Int
  , pipelineGreetingBytes :: Int, pipelineResponseMicros :: Int, pipelineChunkChars :: Int
  , pipelineAbbreviations :: [Text], pipelineHistoryMessages :: Int
  , pipelineWatchdogMicros :: Int, pipelineResponseBytes :: Int, pipelineSpeechBytes :: Int
  } deriving (Eq, Show, Generic)
instance FromJSON PipelineRules where parseJSON = genericParseJSON (jsonOptions 8)

data Pipeline = Pipeline
  { pipelineSettings :: Settings, pipelineRules :: PipelineRules, pipelineSend :: StreamSend
  , pipelineGreeting :: BS.ByteString, pipelineTimeout :: Int, pipelineSilence :: Int
  , pipelineMaximum :: Int, pipelineTokens :: Int, pipelineMessageBytes :: Int
  }

data TurnState = Listening [BS.ByteString] Int | Recording Int Int
               | Committed Double Text | Replying | Echo Double deriving (Eq, Show)
data TurnAction = Begin BS.ByteString | Append BS.ByteString | Commit deriving (Eq, Show)

stepFrame :: PipelineRules -> Int -> Int -> Double -> Bool -> BS.ByteString -> TurnState -> (TurnState, [TurnAction])
stepFrame rules silence maximumFrames now voiced frame state = case state of
  Listening frames run -> begin frames run
  Recording count quiet -> record count quiet
  Echo deadline | now >= deadline -> begin [] 0
  _ -> (state, [])
  where
    begin frames run = let buffered = keepLast (pipelinePrerollFrames rules) (frames ++ [frame])
                           nextRun = if voiced then run + 1 else 0
                       in if nextRun >= pipelineStartFrames rules
                          then (Recording (length buffered) 0, [Begin (BS.concat buffered)])
                          else (Listening buffered nextRun, [])
    record count quiet = let nextCount = count + 1
                             nextQuiet = if voiced then 0 else quiet + 1
                             done = nextQuiet >= silence || nextCount >= maximumFrames
                         in if done then (Committed now "", [Append frame, Commit])
                            else (Recording nextCount nextQuiet, [Append frame])

keepLast :: Int -> [a] -> [a]
keepLast count values = drop (max 0 (length values - count)) values

preparePipeline :: Settings -> IO (Result Pipeline)
preparePipeline settings = case pipelineConfiguration settings of
  Left err -> pure (Left err)
  Right (rules, duration, silence, maximumFrames, tokens, limit) -> do
    sender <- newStreamSender
    let initial = Pipeline settings rules sender BS.empty duration silence maximumFrames tokens limit
    greeting <- buildGreeting initial
    pure (Right initial {pipelineGreeting = fromRight BS.empty greeting})

pipelineConfiguration :: Settings -> Result (PipelineRules, Int, Int, Int, Int, Int)
pipelineConfiguration settings = do
  rules <- ruleSection settings "pipeline"
  duration <- numericSetting settings "OPENAI_TIMEOUT_SECONDS"
  silence <- integerSetting settings "VAD_SILENCE_MS"
  maximumSeconds <- numericSetting settings "MAX_UTTERANCE_SECONDS"
  tokens <- integerSetting settings "OPENAI_CHAT_MAX_OUTPUT_TOKENS"
  limit <- rule settings "runtime" "websocket_message_bytes"
  let frameSeconds = fromIntegral (pipelineFrameMicros rules) / 1000000
      quietFrames = ceiling (fromIntegral silence / 1000 / frameSeconds)
      maximumFrames = ceiling (maximumSeconds / frameSeconds)
  pure (rules, ceiling (duration * 1000000), quietFrames, maximumFrames, tokens, limit)

nowSeconds :: IO Double
nowSeconds = (\nanos -> fromIntegral nanos / 1000000000) <$> getMonotonicTimeNSec

pipelineSession :: Pipeline -> Report -> Text -> Socket -> IO (Result ())
pipelineSession pipeline report identifier socket = either (pure . Left) start configured
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
      if accepted then pumpInput transport queue ready else pure (Left (failure "pipeline_input_overflow"))

queueInput :: TBQueue BS.ByteString -> TVar Bool -> BS.ByteString -> STM Bool
queueInput queue ready bytes = do
  accept <- readTVar ready
  full <- isFullTBQueue queue
  if not accept then pure True else
    if full then pure False else writeTBQueue queue bytes >> pure True

startPipeline :: Pipeline -> Media -> TBQueue BS.ByteString -> TVar Bool -> TVar Bool -> WS.Connection -> IO (Result ())
startPipeline pipeline transport queue ready started conn = do
  let setup = sendSTT pipeline conn (transcriptionConfig (setting (pipelineSettings pipeline) "OPENAI_STT_MODEL")) >>= await
      await = either (pure . Left) (const (awaitUpdated conn))
  connected <- timeout (pipelineTimeout pipeline) setup
  either (pure . Left) (const run) (fromMaybe (Left (failure "transcription_setup_timeout")) connected)
  where
    run = do
      atomically (writeTVar started True)
      atomically (readTVar ready >>= check)
      runPipeline pipeline transport queue conn

pipelineOpening :: Pipeline -> Media -> TVar Bool -> IO (Result ())
pipelineOpening pipeline transport ready = do
  result <- playBytes rules transport (pipelineGreeting pipeline)
  either (pure . Left) (const finished) result
  where
    rules = pipelineRules pipeline
    finished = do
      if BS.null (pipelineGreeting pipeline) then pure () else threadDelay (pipelineEchoMicros rules)
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
  result <- foldM processFrame (Right ()) frames
  either (pure . Left) (const (consumeInput pipeline queue conn vad converter state rest)) result
  where
    processFrame (Left err) _ = pure (Left err)
    processFrame (Right ()) frame = acceptFrame pipeline conn vad converter state frame

acceptFrame :: Pipeline -> WS.Connection -> VAD -> Resampler -> TVar TurnState -> BS.ByteString -> IO (Result ())
acceptFrame pipeline conn vad converter state frame = do
  now <- nowSeconds
  current <- readTVarIO state
  voiced <- classifyFrame (pipelineRules pipeline) vad current now frame
  actions <- atomically (transitionFrame pipeline state now voiced frame)
  either (pure . Left) (foldM run (Right ())) actions
  where
    run (Left err) _ = pure (Left err)
    run (Right ()) action = performTurnAction pipeline conn converter action

classifyFrame :: PipelineRules -> VAD -> TurnState -> Double -> BS.ByteString -> IO (Result Bool)
classifyFrame rules vad state now frame = case state of
  Listening _ _ -> classify
  Recording _ _ -> classify
  Echo deadline | now >= deadline -> resetVAD vad (pipelineVadMode rules) >>= either (pure . Left) (const classify)
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
  either (pure . Left) (const (performTurnAction pipeline conn converter (Append bytes))) reset
performTurnAction pipeline conn converter (Append bytes) =
  resample converter bytes False >>= either (pure . Left) (sendPCM pipeline conn)
performTurnAction pipeline conn converter Commit = do
  final <- resample converter BS.empty True
  sent <- either (pure . Left) (sendPCM pipeline conn) final
  either (pure . Left) (const (sendSTT pipeline conn (object ["type" .= ("input_audio_buffer.commit" :: Text)]))) sent

sendPCM :: Pipeline -> WS.Connection -> BS.ByteString -> IO (Result ())
sendPCM _ _ bytes | BS.null bytes = pure (Right ())
sendPCM pipeline conn bytes = sendSTT pipeline conn command
  where
    encoded = TE.decodeUtf8 (B64.encode bytes)
    command = object ["type" .= ("input_audio_buffer.append" :: Text), "audio" .= encoded]

sendSTT :: Pipeline -> WS.Connection -> Value -> IO (Result ())
sendSTT pipeline conn value = do
  sent <- timeout (pipelineTimeout pipeline) (WS.sendTextData conn (encode value))
  pure (maybe (Left (failure "transcription_send_timeout")) Right sent)

receiveSTT :: WS.Connection -> IO (Result Value)
receiveSTT conn = do
  bytes <- WS.receiveData conn
  pure (decodeBytes "invalid_transcription_event" bytes >>= validateSTT)

validateSTT :: Value -> Result Value
validateSTT value@(Object fields) = case KM.lookup "type" fields of
  Just (String kind) | kind == "error" || ".failed" `T.isSuffixOf` kind -> Left (failure "transcription_provider_error")
  Just (String _) -> Right value
  _ -> Left (failure "invalid_transcription_event")
validateSTT _ = Left (failure "invalid_transcription_event")

receiveTurns :: Pipeline -> Media -> WS.Connection -> TVar TurnState -> IORef [Value] -> IO (Result ())
receiveTurns pipeline transport conn state history = do
  event <- receiveSTT conn
  accepted <- either (pure . Left) (atomically . acceptTranscript state) event
  result <- either (pure . Left) (maybe (pure (Right ())) (respondTurn pipeline transport state history)) accepted
  either (pure . Left) (const (receiveTurns pipeline transport conn state history)) result

acceptTranscript :: TVar TurnState -> Value -> STM (Result (Maybe Text))
acceptTranscript state (Object fields) = do
  current <- readTVar state
  case (KM.lookup "type" fields, current, KM.lookup "item_id" fields) of
    (Just (String "input_audio_buffer.committed"), Committed stamp "", Just (String ident)) ->
      writeTVar state (Committed stamp ident) >> pure (Right Nothing)
    (Just (String "conversation.item.input_audio_transcription.completed"), Committed _ ident, Just (String received))
      | ident == received && ident /= "" -> complete (KM.lookup "transcript" fields)
    (Just (String kind), _, _) | kind `elem` ["input_audio_buffer.committed", "conversation.item.input_audio_transcription.completed"] ->
      pure (Left (failure "unexpected_transcription_turn"))
    _ -> pure (Right Nothing)
  where
    complete (Just (String text)) = writeTVar state Replying >> pure (Right (Just text))
    complete _ = pure (Left (failure "invalid_transcription_event"))
acceptTranscript _ _ = pure (Left (failure "invalid_transcription_event"))

respondTurn :: Pipeline -> Media -> TVar TurnState -> IORef [Value] -> Text -> IO (Result ())
respondTurn pipeline transport state history text = do
  observeEvent transport (object ["type" .= ("session.input_transcript.delta" :: Text), "delta" .= text])
  result <- timeout (pipelineResponseMicros (pipelineRules pipeline)) (respond pipeline transport history text)
  now <- nowSeconds
  let deadline = now + fromIntegral (pipelineEchoMicros (pipelineRules pipeline)) / 1000000
  atomically (writeTVar state (Echo deadline))
  pure (fromMaybe (Left (failure "pipeline_response_timeout")) result)

transcriptWatchdog :: Pipeline -> TVar TurnState -> IO (Result ())
transcriptWatchdog pipeline state = do
  now <- nowSeconds
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
    let failed = wait worker >>= either (pure . Left) (const (atomically retry))
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
  case responseRequest pipeline (previous ++ [user]) of
    Left err -> pure (Left err)
    Right request -> do
      result <- pipelineSend pipeline request (consumeChat pipeline parsed speakText)
      (_, complete) <- readIORef parsed
      observeEvent transport (object ["type" .= ("session.output_transcript.delta" :: Text), "delta" .= chatText complete])
      finishChat pipeline history (previous ++ [user]) complete result

finishChat :: Pipeline -> IORef [Value] -> [Value] -> ChatState -> Result () -> IO (Result ())
finishChat _ _ _ _ (Left err) = pure (Left err)
finishChat pipeline history previous state (Right ())
  | not (chatCompleted state) = pure (Left (failure "chat_response_incomplete"))
  | otherwise = do
      let assistant = object ["role" .= ("assistant" :: Text), "content" .= chatText state]
          next = keepLast (pipelineHistoryMessages (pipelineRules pipeline)) (previous ++ [assistant])
      writeIORef history next
      pure (Right ())

consumeChat :: Pipeline -> IORef (BS.ByteString, ChatState) -> (Text -> IO (Result ())) -> BS.ByteString -> IO (Result ())
consumeChat pipeline parsed speakText bytes = do
  (pending, state) <- readIORef parsed
  case splitEvents (pending <> bytes) of
    Left err -> pure (Left err)
    Right (events, rest) -> do
      result <- foldM advance (Right state) events
      either (pure . Left) (\next -> writeIORef parsed (rest, next) >> pure (Right ())) result
  where
    rules = pipelineRules pipeline
    chunks = ChunkRules (pipelineChunkChars rules) (pipelineAbbreviations rules)
    advance (Left err) _ = pure (Left err)
    advance (Right current) event = case chatEvent chunks event current of
      Left err -> pure (Left err)
      Right (next, pieces) -> do
        spoken <- foldM (\result piece -> either (pure . Left) (const (speakText piece)) result) (Right ()) pieces
        pure (next <$ spoken)

responseRequest :: Pipeline -> [Value] -> Result Request
responseRequest pipeline history = do
  instructions <- configuredText (pipelineSettings pipeline) "PHONE_ASSISTANT_CHAT_INSTRUCTIONS" "chat_instructions"
  let settings = pipelineSettings pipeline
      body = chatRequest (setting settings "OPENAI_CHAT_MODEL") instructions
        (setting settings "OPENAI_CHAT_REASONING_EFFORT") (pipelineTokens pipeline) history
  let rules = pipelineRules pipeline
  pure (providerRequest pipeline (pipelineResponsesUrl rules) body (pipelineResponseBytes rules))

configuredText :: Settings -> Text -> Text -> Result Text
configuredText settings key fallback
  | T.null value = rule settings "settings" fallback
  | otherwise = Right value
  where
    value = setting settings key

providerRequest :: Pipeline -> Text -> Value -> Int -> Request
providerRequest pipeline url body limit = Request "POST" url headers [] (JsonBody body) limit duration
  where
    headers = [("Authorization", "Bearer " <> TE.encodeUtf8 (setting (pipelineSettings pipeline) "OPENAI_API_KEY"))]
    duration = ceiling (fromIntegral (pipelineTimeout pipeline) / (1000000 :: Double))

speak :: Pipeline -> (Result (Maybe BS.ByteString) -> IO (Result ())) -> Text -> IO (Result ())
speak pipeline enqueue text = pipelineSend pipeline request consume
  where
    settings = pipelineSettings pipeline
    body = speechRequest (setting settings "OPENAI_TTS_MODEL") (setting settings "OPENAI_TTS_VOICE")
      (setting settings "OPENAI_TTS_INSTRUCTIONS") text
    rules = pipelineRules pipeline
    request = providerRequest pipeline (pipelineSpeechUrl rules) body (pipelineSpeechBytes rules)
    chunkBytes = pipelineFrameBytes rules * pipelineProviderRate rules `div` pipelineInputRate rules
    consume bytes = foldM send (Right ()) (sizedChunks chunkBytes bytes)
    send (Left err) _ = pure (Left err)
    send (Right ()) bytes = enqueue (Right (Just bytes))

sizedChunks :: Int -> BS.ByteString -> [BS.ByteString]
sizedChunks _ bytes | BS.null bytes = []
sizedChunks size bytes = let (first, rest) = BS.splitAt size bytes in first : sizedChunks size rest

playQueue :: PipelineRules -> Media -> Resampler -> TBQueue (Result (Maybe BS.ByteString)) -> BS.ByteString -> IO (Result ())
playQueue rules transport converter queue pending = do
  next <- atomically (readTBQueue queue)
  case next of
    Left err -> pure (Left err)
    Right Nothing -> finalAudio rules transport converter pending
    Right (Just bytes) -> do
      converted <- resample converter bytes False
      result <- either (pure . Left) (playBuffered rules transport pending) converted
      either (pure . Left) (playQueue rules transport converter queue) result

playBuffered :: PipelineRules -> Media -> BS.ByteString -> BS.ByteString -> IO (Result BS.ByteString)
playBuffered rules transport pending pcm = do
  let (frames, rest) = splitFrames (pipelineFrameBytes rules) (pending <> pcm)
  played <- foldM send (Right ()) frames
  pure (rest <$ played)
  where
    send (Left err) _ = pure (Left err)
    send (Right ()) frame = playFrame rules transport frame

playFrame :: PipelineRules -> Media -> BS.ByteString -> IO (Result ())
playFrame rules transport frame = do
  started <- getMonotonicTimeNSec
  result <- sendAudio transport frame
  ended <- getMonotonicTimeNSec
  let remaining = max 0 (pipelineFrameMicros rules - fromIntegral ((ended - started) `div` 1000))
  threadDelay remaining
  pure (void result)

finalAudio :: PipelineRules -> Media -> Resampler -> BS.ByteString -> IO (Result ())
finalAudio rules transport converter pending = do
  final <- resample converter BS.empty True
  either (pure . Left) (playBytes rules transport . padFrames rules . (pending <>)) final

padFrames :: PipelineRules -> BS.ByteString -> BS.ByteString
padFrames rules bytes = bytes <> BS.replicate amount 0
  where
    size = pipelineFrameBytes rules
    amount = (size - BS.length bytes `mod` size) `mod` size

playBytes :: PipelineRules -> Media -> BS.ByteString -> IO (Result ())
playBytes rules transport bytes = fmap void (playBuffered rules transport BS.empty (padFrames rules bytes))

buildGreeting :: Pipeline -> IO (Result BS.ByteString)
buildGreeting pipeline
  | T.null (T.strip text) = pure (Right BS.empty)
  | otherwise = withResampler (pipelineProviderRate rules) (pipelineInputRate rules) $ \converter -> do
      collected <- newIORef BS.empty
      generated <- timeout (pipelineTimeout pipeline) (speak pipeline (collectGreeting rules converter collected) text)
      final <- resample converter BS.empty True
      before <- readIORef collected
      pure (fromMaybe (Left (failure "greeting_timeout")) generated >> final >>= validateGreeting rules before)
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

validateGreeting :: PipelineRules -> BS.ByteString -> BS.ByteString -> Result BS.ByteString
validateGreeting rules before after
  | BS.length complete > pipelineGreetingBytes rules = Left (failure "greeting_too_large")
  | otherwise = Right complete
  where
    complete = before <> after
