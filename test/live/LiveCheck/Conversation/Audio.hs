module LiveCheck.Conversation.Audio (synthesise, telephoneAudio, spoken, speechPair) where

import Baerbel.Core.Config (Settings, numericSetting, rule, ruleSection, setting)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.HTTP (Request, Response(..), responseStatusResult)
import Baerbel.Core.Pipeline (PipelineRules(..), providerRequest)
import Baerbel.Core.Speech (speechRequest)
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.HTTP (RawSend)
import Baerbel.Runtime.Speech (resample, withResampler)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as Bytes

synthesise :: Settings -> RawSend -> Text -> IO (Result ByteString)
synthesise settings sender text = continue send (speechCall settings text)
  where
    send request = fmap (>>= speechPCM) (sender request)

speechCall :: Settings -> Text -> Result Request
speechCall settings text = do
  rules <- ruleSection settings "pipeline"
  seconds <- numericSetting settings "OPENAI_TIMEOUT_SECONDS"
  let duration = ceiling (seconds * 1000000)
      body = speechRequest (setting settings "OPENAI_TTS_MODEL")
        (setting settings "OPENAI_TTS_VOICE") (setting settings "OPENAI_TTS_INSTRUCTIONS") text
  Right (providerRequest settings duration (pipelineSpeechUrl rules) body (pipelineSpeechBytes rules))

speechPCM :: Response -> Result ByteString
speechPCM response = responseStatusResult (const False) (responseStatus response) >> validPCM bytes
  where bytes = responseBody response

validPCM :: ByteString -> Result ByteString
validPCM bytes
  | Bytes.null bytes || odd (Bytes.length bytes) = Left (failure "live_check_invalid_speech_pcm")
  | otherwise = Right bytes

telephoneAudio :: Settings -> ByteString -> IO (Result ByteString)
telephoneAudio settings bytes = continue convert rates
  where
    rates = (,) <$> rule settings "runtime" "provider_rate" <*> rule settings "runtime" "input_rate"
    convert (input, output) = withResampler input output (\converter -> resample converter bytes True)

spoken :: Settings -> RawSend -> Text -> IO (Result ByteString)
spoken settings sender text = synthesise settings sender text >>= continue (telephoneAudio settings)

speechPair :: Settings -> RawSend -> Text -> Text -> IO (Result (ByteString, ByteString))
speechPair settings sender firstText secondText = spoken settings sender firstText >>= continue second
  where second firstClip = fmap ((,) firstClip <$>) (spoken settings sender secondText)
