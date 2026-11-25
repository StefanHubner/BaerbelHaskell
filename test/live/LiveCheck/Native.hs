module LiveCheck.Native (nativeChecks, decoder) where

import Baerbel.Core.Config (Settings, rule, ruleSection)
import Baerbel.Core.Encoder (Encoding(..))
import Baerbel.Core.Error (Result)
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.NoteMail (NoteMailRules(..))
import Baerbel.Core.Pipeline (PipelineRules(..))
import Baerbel.Core.Speech (audiblePCM, splitFrames)
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.Encoder (encodeAudio)
import Baerbel.Runtime.HTTP (RawSend, newRawSender)
import Baerbel.Runtime.VAD (isSpeech, withVAD)
import Data.Aeson (ToJSON(..), Value, genericToJSON)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import LiveCheck.Check (CheckResult, check, ensure, measured)
import LiveCheck.Conversation.Audio (synthesise, telephoneAudio)
import qualified Data.ByteString as Bytes

data AudioReport = AudioReport
  { audioSpeechSeconds :: Double, audioDurationSeconds :: Double
  , audioProcessingSeconds :: Double, audioRealtimeRatio :: Double
  , audioSpeechFrames :: Int, audioMp3Bytes :: Int
  , audioDecodedSeconds :: Double } deriving (Generic)

instance ToJSON AudioReport where toJSON = genericToJSON (jsonOptions 5)

nativeChecks :: Settings -> IO [CheckResult]
nativeChecks settings = do
  sender <- newRawSender
  result <- check "audio.tts_resample_vad_mp3" (nativeRoundTrip settings sender)
  pure [result]

nativeRoundTrip :: Settings -> RawSend -> IO (Result Value)
nativeRoundTrip settings sender = do
  let text = "This is a synthetic audio check. The clear blue sky stretches above the quiet harbour."
  (seconds, speech) <- measured (synthesise settings sender text)
  pcm <- continue (telephoneAudio settings) speech
  continue (inspectAudio settings seconds) pcm

inspectAudio :: Settings -> Double -> ByteString -> IO (Result Value)
inspectAudio settings seconds pcm = continue inspect configured
  where
    configured = (,) <$> ruleSection settings "pipeline" <*> rule settings "notes" "mail"
    inspect (pipeline, mail) = do
      (processing, result) <- measured (roundTrip pipeline (noteMailEncoding mail) pcm)
      pure (result >>= audioReport pipeline seconds processing pcm)

roundTrip :: PipelineRules -> Encoding -> ByteString -> IO (Result (Int, Int, ByteString))
roundTrip rules encoding pcm = do
  frames <- voicedFrames rules pcm
  mp3 <- continue (const (encodeAudio encoding pcm)) frames
  decoded <- continue (encodeAudio (decoder encoding)) mp3
  pure ((,,) <$> frames <*> fmap Bytes.length mp3 <*> decoded)

voicedFrames :: PipelineRules -> ByteString -> IO (Result Int)
voicedFrames rules pcm = withVAD (pipelineVadMode rules) classify
  where
    (frames, _) = splitFrames (pipelineFrameBytes rules) pcm
    classify vad = do
      results <- traverse (isSpeech vad (pipelineInputRate rules)) frames
      pure (count <$> sequence results)
    count = length . filter id

decoder :: Encoding -> Encoding
decoder encoding = encoding {encodingArguments = arguments}
  where arguments = ["-hide_banner", "-loglevel", "error", "-nostdin", "-f", "mp3", "-i", "pipe:0",
          "-f", "s16le", "-ar", "8000", "-ac", "1", "pipe:1"]

audioReport :: PipelineRules -> Double -> Double -> ByteString -> (Int, Int, ByteString) -> Result Value
audioReport rules speech processing pcm (voiced, mp3Bytes, decoded) = do
  ensure "live_check_audio_not_audible" (audiblePCM pcm && audiblePCM decoded && voiced > 0)
  ensure "live_check_audio_duration_changed" (abs (decodedDuration - duration) < 0.25)
  ensure "live_check_audio_slower_than_realtime" (ratio < 1)
  pure (toJSON report)
  where
    rateBytes = fromIntegral (pipelineInputRate rules * 2)
    duration = fromIntegral (Bytes.length pcm) / rateBytes
    decodedDuration = fromIntegral (Bytes.length decoded) / rateBytes
    ratio = processing / duration
    report = AudioReport speech duration processing ratio voiced mp3Bytes decodedDuration
