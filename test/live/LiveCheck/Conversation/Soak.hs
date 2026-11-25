module LiveCheck.Conversation.Soak (soakCheck) where

import Baerbel.Core.Config (Settings)
import Baerbel.Core.Error (Result)
import Baerbel.Core.JSON (field, jsonOptions, numberField)
import Baerbel.Core.Live (LiveConfig(..))
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.HTTP (RawSend)
import Baerbel.Runtime.Services (dispatch)
import Data.Aeson (ToJSON(..), Value, genericToJSON, object, (.=))
import Data.ByteString (ByteString)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import LiveCheck.Check (emit, ensure)
import LiveCheck.Conversation.Audio (speechPair)
import LiveCheck.Conversation.Model
import LiveCheck.Conversation.Session

soakCheck :: Settings -> RawSend -> IO (Result Value)
soakCheck settings sender = speechPair settings sender firstText nextText >>= continue run
  where
    firstText = "What is two plus two? Please answer in one short sentence."
    nextText = "I have another question. What is three plus three? Please answer in one short sentence."
    run clips = do
      result <- withConversation settings [] (const (dispatch [])) (soakAction clips)
      pure (result >>= boundedOutput)

soakAction :: (ByteString, ByteString) -> Running -> IO (Result Value)
soakAction (firstClip, nextClip) running = do
  started <- getMonotonicTimeNSec
  before <- snapshot running
  firstAnswer <- answer running firstClip ["four", "4"]
  continue (const (lateFollowup running started before nextClip)) firstAnswer

lateFollowup :: Running -> Word64 -> Trace -> ByteString -> IO (Result Value)
lateFollowup running started before clip = do
  waiting <- untilElapsed running started 190
  continue (const followup) waiting
  where
    followup = answer running clip ["six", "6"] >>= continue (finishSoak running started before)

finishSoak :: Running -> Word64 -> Trace -> Value -> IO (Result Value)
finishSoak running started before timing = do
  result <- untilElapsed running started 210
  ended <- getMonotonicTimeNSec
  let rate = liveProviderRate (harnessConfig (runningHarness running))
  continue (reportSoak rate started ended before timing) result

untilElapsed :: Running -> Word64 -> Word64 -> IO (Result Trace)
untilElapsed running started seconds = await running "live_check_soak_ended_early" 230000000 elapsed
  where
    deadline = started + seconds * 1000000000
    elapsed now _ = now >= deadline

data InputTiming = InputTiming
  { inputWallMs :: Double, inputPcmBytes :: Int, inputAudioMs :: Double, inputDeficitMs :: Double
  } deriving (Generic)
instance ToJSON InputTiming where toJSON = genericToJSON (jsonOptions 5)

measureInput :: Int -> Word64 -> Word64 -> Trace -> Trace -> InputTiming
measureInput rate started ended before trace = InputTiming duration bytes audio (duration - audio)
  where
    duration = milliseconds started ended
    bytes = traceInputBytes trace - traceInputBytes before
    audio = fromIntegral bytes * 1000 / fromIntegral (rate * 2)

reportSoak :: Int -> Word64 -> Word64 -> Trace -> Value -> Trace -> IO (Result Value)
reportSoak rate started ended before timing trace = do
  let measured = measureInput rate started ended before trace
  emit (object ["diagnostic" .= ("soak_input_clock" :: String), "measurements" .= measured])
  pure (soakResult measured timing trace)

soakResult :: InputTiming -> Value -> Trace -> Result Value
soakResult measured timing trace = do
  ensure "live_check_soak_clock_drift" (abs (inputDeficitMs measured) <= 1000)
  ensure "live_check_soak_unexpected_tool" (null (calls trace))
  Right (object ["input_clock" .= measured, "late_followup" .= timing])

boundedOutput :: Value -> Result Value
boundedOutput value = do
  let queued = field "audio" value >>= numberField "max_output_queue_ms"
  ensure "live_check_soak_output_backlog" (maybe False (<= 2000) queued)
  Right value
