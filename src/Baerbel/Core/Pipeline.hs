module Baerbel.Core.Pipeline where

import Baerbel.Core.Chat
import Baerbel.Core.Config (Settings, setting, rule, integerSetting, numericSetting, ruleSection)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.HTTP (Request(..), Body(..))
import Baerbel.Core.JSON (jsonOptions)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

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
    begin frames run
      | nextRun >= pipelineStartFrames rules = (Recording (length buffered) 0, [Begin (BS.concat buffered)])
      | otherwise = (Listening buffered nextRun, [])
      where
        buffered = keepLast (pipelinePrerollFrames rules) (frames ++ [frame])
        nextRun | voiced = run + 1
                | otherwise = 0
    record count quiet
      | nextQuiet >= silence || nextCount >= maximumFrames = (Committed now "", [Append frame, Commit])
      | otherwise = (Recording nextCount nextQuiet, [Append frame])
      where
        nextCount = count + 1
        nextQuiet | voiced = 0
                  | otherwise = quiet + 1

keepLast :: Int -> [a] -> [a]
keepLast count values = drop (max 0 (length values - count)) values

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

validateSTT :: Value -> Result Value
validateSTT value@(Object fields) = case KM.lookup "type" fields of
  Just (String kind) | kind == "error" || ".failed" `T.isSuffixOf` kind -> Left (failure "transcription_provider_error")
  Just (String _) -> Right value
  _ -> Left (failure "invalid_transcription_event")
validateSTT _ = Left (failure "invalid_transcription_event")

transcriptStep :: Value -> TurnState -> Result (TurnState, Maybe Text)
transcriptStep (Object fields) current = case (KM.lookup "type" fields, current, KM.lookup "item_id" fields) of
  (Just (String "input_audio_buffer.committed"), Committed stamp "", Just (String ident)) ->
    Right (Committed stamp ident, Nothing)
  (Just (String "conversation.item.input_audio_transcription.completed"), Committed _ ident, Just (String received))
    | ident == received && ident /= "" -> complete (KM.lookup "transcript" fields)
  (Just (String kind), _, _) | kind `elem` ["input_audio_buffer.committed", "conversation.item.input_audio_transcription.completed"] ->
    Left (failure "unexpected_transcription_turn")
  _ -> Right (current, Nothing)
  where
    complete (Just (String text)) = Right (Replying, Just text)
    complete _ = Left (failure "invalid_transcription_event")
transcriptStep _ _ = Left (failure "invalid_transcription_event")

finishChat :: Int -> [Value] -> ChatState -> Result () -> Result [Value]
finishChat _ _ _ (Left err) = Left err
finishChat limit previous state (Right ())
  | not (chatCompleted state) = Left (failure "chat_response_incomplete")
  | otherwise = Right (keepLast limit (previous ++ [assistant]))
  where assistant = object ["role" .= ("assistant" :: Text), "content" .= chatText state]

responseRequest :: Settings -> PipelineRules -> Int -> Int -> [Value] -> Result Request
responseRequest settings rules duration tokens history = do
  instructions <- configuredText settings "PHONE_ASSISTANT_CHAT_INSTRUCTIONS" "chat_instructions"
  let body = chatRequest (setting settings "OPENAI_CHAT_MODEL") instructions
        (setting settings "OPENAI_CHAT_REASONING_EFFORT") tokens history
  pure (providerRequest settings duration (pipelineResponsesUrl rules) body (pipelineResponseBytes rules))

configuredText :: Settings -> Text -> Text -> Result Text
configuredText settings key fallback
  | T.null value = rule settings "settings" fallback
  | otherwise = Right value
  where
    value = setting settings key

providerRequest :: Settings -> Int -> Text -> Value -> Int -> Request
providerRequest settings micros url body limit = Request "POST" url headers [] (JsonBody body) limit duration
  where
    headers = [("Authorization", "Bearer " <> TE.encodeUtf8 (setting settings "OPENAI_API_KEY"))]
    duration = ceiling (fromIntegral micros / (1000000 :: Double))

sizedChunks :: Int -> BS.ByteString -> [BS.ByteString]
sizedChunks _ bytes | BS.null bytes = []
sizedChunks size bytes = let (first, rest) = BS.splitAt size bytes in first : sizedChunks size rest

padFrames :: PipelineRules -> BS.ByteString -> BS.ByteString
padFrames rules bytes = bytes <> BS.replicate amount 0
  where
    size = pipelineFrameBytes rules
    amount = (size - BS.length bytes `mod` size) `mod` size

validateGreeting :: PipelineRules -> BS.ByteString -> BS.ByteString -> Result BS.ByteString
validateGreeting rules before after
  | BS.length complete > pipelineGreetingBytes rules = Left (failure "greeting_too_large")
  | otherwise = Right complete
  where
    complete = before <> after
