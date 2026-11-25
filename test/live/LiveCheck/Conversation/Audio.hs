module LiveCheck.Conversation.Audio (Speech, synthesise, telephoneAudio, spoken, speechPair) where

import Baerbel.Core.Config (Settings, rule, setting)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.HTTP (Request(..), Response(..), responseStatusResult)
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.Provider (post)
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.HTTP (RawSend)
import Baerbel.Runtime.Speech (resample, withResampler)
import Data.Aeson (ToJSON(..), genericToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import LiveCheck.Config (SpeechSettings(..))
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as Bytes

type Speech = Text -> IO (Result ByteString)

data SpeechBody = SpeechBody
  { bodyModel :: Text, bodyVoice :: Text, bodyInput :: Text
  , bodyResponseFormat :: Text, bodyInstructions :: Maybe Text } deriving (Generic)
instance ToJSON SpeechBody where toJSON = genericToJSON (jsonOptions 4)

synthesise :: SpeechSettings -> Settings -> RawSend -> Speech
synthesise rules settings sender text = fmap (>>= speechPCM) (sender request)
  where request = speechCall rules (setting settings "OPENAI_API_KEY") text

speechCall :: SpeechSettings -> Text -> Text -> Request
speechCall rules key text = request {requestHeaders = headers}
  where
    model = speechModel rules
    style | speechInstructions rules == "" || model `elem` ["tts-1", "tts-1-hd"] = Nothing
          | otherwise = Just (speechInstructions rules)
    body = SpeechBody model (speechVoice rules) text "pcm" style
    request = post (speechHttp rules) (speechUrl rules) [] body
    headers = [("Authorization", "Bearer " <> Text.encodeUtf8 key)]

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

spoken :: Settings -> Speech -> Text -> IO (Result ByteString)
spoken settings speech text = speech text >>= continue (telephoneAudio settings)

speechPair :: Settings -> Speech -> Text -> Text -> IO (Result (ByteString, ByteString))
speechPair settings sender firstText secondText = spoken settings sender firstText >>= continue second
  where second firstClip = fmap ((,) firstClip <$>) (spoken settings sender secondText)
