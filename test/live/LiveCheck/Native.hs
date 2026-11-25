module LiveCheck.Native (nativeChecks, decoder) where

import Baerbel.Core.Config (Settings, rule)
import Baerbel.Core.Encoder (Encoding(..))
import Baerbel.Core.Error (Result)
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.NoteMail (NoteMailRules(..))
import Baerbel.Core.NoteRuntime (NoteObserverConfig(..))
import Baerbel.Core.Speech (audiblePCM, splitFrames)
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.Encoder (encodeAudio)
import Baerbel.Runtime.HTTP (newRawSender)
import Baerbel.Runtime.VAD (isSpeech, withVAD)
import Data.Aeson (ToJSON(..), Value, genericToJSON)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import LiveCheck.Check (CheckResult, check, ensure, measured)
import LiveCheck.Config (Development(..))
import LiveCheck.Conversation.Audio (Speech, synthesise, telephoneAudio)
import qualified Data.ByteString as Bytes

data AudioReport = AudioReport
  { audioSpeechSeconds :: Double, audioDurationSeconds :: Double
  , audioProcessingSeconds :: Double, audioRealtimeRatio :: Double
  , audioSpeechFrames :: Int, audioMp3Bytes :: Int
  , audioDecodedSeconds :: Double } deriving (Generic)

instance ToJSON AudioReport where toJSON = genericToJSON (jsonOptions 5)

nativeChecks :: Development -> Settings -> IO [CheckResult]
nativeChecks development settings = do
  sender <- newRawSender
  let speech = synthesise (developmentSpeech development) settings sender
  result <- check "audio.tts_resample_vad_mp3" (nativeRoundTrip settings speech)
  pure [result]

nativeRoundTrip :: Settings -> Speech -> IO (Result Value)
nativeRoundTrip settings sender = do
  let text = "This is a synthetic audio check. The clear blue sky stretches above the quiet harbour."
  (seconds, speech) <- measured (sender text)
  pcm <- continue (telephoneAudio settings) speech
  continue (inspectAudio settings seconds) pcm

inspectAudio :: Settings -> Double -> ByteString -> IO (Result Value)
inspectAudio settings seconds pcm = continue inspect configured
  where
    configured = (,) <$> rule settings "notes" "observer" <*> rule settings "notes" "mail"
    inspect (observer, mail) = do
      (processing, result) <- measured (roundTrip observer (noteMailEncoding mail) pcm)
      pure (result >>= audioReport observer seconds processing pcm)

roundTrip :: NoteObserverConfig -> Encoding -> ByteString -> IO (Result (Int, Int, ByteString))
roundTrip rules encoding pcm = do
  frames <- voicedFrames rules pcm
  mp3 <- continue (const (encodeAudio encoding pcm)) frames
  decoded <- continue (encodeAudio (decoder encoding)) mp3
  pure ((,,) <$> frames <*> fmap Bytes.length mp3 <*> decoded)

voicedFrames :: NoteObserverConfig -> ByteString -> IO (Result Int)
voicedFrames rules pcm = withVAD (observerVadMode rules) classify
  where
    (frames, _) = splitFrames (observerFrameBytes rules) pcm
    classify vad = do
      results <- traverse (isSpeech vad (observerRate rules)) frames
      pure (count <$> sequence results)
    count = length . filter id

decoder :: Encoding -> Encoding
decoder encoding = encoding {encodingArguments = arguments}
  where arguments = ["-hide_banner", "-loglevel", "error", "-nostdin", "-f", "mp3", "-i", "pipe:0",
          "-f", "s16le", "-ar", "8000", "-ac", "1", "pipe:1"]

audioReport :: NoteObserverConfig -> Double -> Double -> ByteString -> (Int, Int, ByteString) -> Result Value
audioReport rules speech processing pcm (voiced, mp3Bytes, decoded) = do
  ensure "live_check_audio_not_audible" (audiblePCM pcm && audiblePCM decoded && voiced > 0)
  ensure "live_check_audio_duration_changed" (abs (decodedDuration - duration) < 0.25)
  ensure "live_check_audio_slower_than_realtime" (ratio < 1)
  pure (toJSON report)
  where
    rateBytes = fromIntegral (observerRate rules * 2)
    duration = fromIntegral (Bytes.length pcm) / rateBytes
    decodedDuration = fromIntegral (Bytes.length decoded) / rateBytes
    ratio = processing / duration
    report = AudioReport speech duration processing ratio voiced mp3Bytes decodedDuration
