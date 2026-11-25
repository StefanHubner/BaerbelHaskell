module Baerbel.Core.AudioMetrics
  ( AudioDelivery(..), AudioMetrics(..), AudioNotice(..), emptyMetrics
  , recordInputFrame, recordQueueDepth, recordDelivery, noticeValue, summaryValue
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Word (Word64)

-- Deferred frames enter the ordinary player's queue during an announcement.
-- Only Delivered means that the transport completed a telephone audio write.
data AudioDelivery = Delivered | Suppressed | Deferred deriving (Eq, Show)

data AudioMetrics = AudioMetrics
  { metricsStart :: Word64, metricsInputFrames :: Int, metricsOutputFrames :: Int
  , metricsMaxQueuedFrames :: Int, metricsFirstAudioMS :: Maybe Double
  , metricsFirstSpeechMS :: Maybe Double
  } deriving (Eq, Show)

data AudioNotice = AudioStarted Double | SpeechStarted Double deriving (Eq, Show)

emptyMetrics :: Word64 -> AudioMetrics
emptyMetrics started = AudioMetrics started 0 0 0 Nothing Nothing

recordInputFrame :: AudioMetrics -> AudioMetrics
recordInputFrame metrics = metrics {metricsInputFrames = metricsInputFrames metrics + 1}

recordQueueDepth :: Int -> AudioMetrics -> AudioMetrics
recordQueueDepth count metrics = metrics {metricsMaxQueuedFrames = max count (metricsMaxQueuedFrames metrics)}

recordDelivery :: Word64 -> AudioDelivery -> Bool -> AudioMetrics -> (AudioMetrics, [AudioNotice])
recordDelivery now Delivered audible metrics = (updated, audioNotice ++ speechNotice)
  where
    elapsed :: Double
    elapsed | now < metricsStart metrics = 0
            | otherwise = fromIntegral (now - metricsStart metrics) / 1000000
    millis :: Double
    millis = fromIntegral (round (elapsed * 100) :: Integer) / 100
    firstSpeech :: Maybe Double
    firstSpeech | audible = metricsFirstSpeechMS metrics <|> Just millis
                | otherwise = metricsFirstSpeechMS metrics
    updated :: AudioMetrics
    updated = metrics {metricsOutputFrames = metricsOutputFrames metrics + 1,
      metricsFirstAudioMS = metricsFirstAudioMS metrics <|> Just millis, metricsFirstSpeechMS = firstSpeech}
    audioNotice :: [AudioNotice]
    audioNotice = [AudioStarted millis | isNothing (metricsFirstAudioMS metrics)]
    speechNotice :: [AudioNotice]
    speechNotice = [SpeechStarted millis | audible && isNothing (metricsFirstSpeechMS metrics)]
recordDelivery _ _ _ metrics = (metrics, [])

noticeValue :: AudioNotice -> Value
noticeValue (AudioStarted millis) = object ["type" .= ("phone.audio_started" :: Text), "first_audio_ms" .= millis]
noticeValue (SpeechStarted millis) = object ["type" .= ("phone.speech_started" :: Text), "first_non_silent_audio_ms" .= millis]

summaryValue :: Int -> AudioMetrics -> Value
summaryValue frameMicros metrics = object
  ["type" .= ("phone.call_summary" :: Text), "input_audio_ms" .= inputMillis,
   "output_audio_ms" .= outputMillis, "max_output_queue_ms" .= queuedMillis,
   "first_audio_ms" .= metricsFirstAudioMS metrics, "first_non_silent_audio_ms" .= metricsFirstSpeechMS metrics]
  where
    frameMillis :: Double
    frameMillis = fromIntegral frameMicros / 1000
    inputMillis :: Double
    inputMillis = fromIntegral (metricsInputFrames metrics) * frameMillis
    outputMillis :: Double
    outputMillis = fromIntegral (metricsOutputFrames metrics) * frameMillis
    queuedMillis :: Double
    queuedMillis = fromIntegral (metricsMaxQueuedFrames metrics) * frameMillis
