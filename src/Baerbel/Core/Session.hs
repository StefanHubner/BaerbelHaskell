{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Session where

import Baerbel.Core.Config (Settings, setting, rule, booleanSetting, integerSetting, numericSetting, toolDefinitions)
import Baerbel.Core.EndCall (EndConfig(..))
import Baerbel.Core.Telephony (TelephonyConfig(..))
import Baerbel.Core.Routing (RouteSettings(..))
import Baerbel.Core.NoteRuntime (NoteObserverConfig(..))
import qualified Baerbel.Core.Pin as Pin
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.Live (LiveConfig(..), ToolLimits(..))
import Baerbel.Core.Protocol (SessionRole(..), SessionSettings(..), sessionConfig, voiceIdentity, greetingCommand)
import Data.Aeson (Value, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T

liveConfiguration :: Settings -> SessionSettings -> Text -> Result LiveConfig
liveConfiguration settings session greeting = do
  template <- rule settings "protocol" "greetingPrefix"
  limits <- toolLimits settings
  seconds <- numericSetting settings "OPENAI_TIMEOUT_SECONDS"
  toolMicros <- rule settings "runtime" "tool_micros"
  directSeconds <- integerSetting settings "PHONE_ASSISTANT_DIRECT_CALL_TIMEOUT"
  extra <- rule settings "runtime" "direct_extra_micros"
  let callMicros = directSeconds * 1000000 + extra
      timeoutMicros = ceiling (seconds * 1000000)
      ordinaryMicros = min toolMicros timeoutMicros
  let configuration = sessionConfig session
      opening = greetingCommand template greeting
      runtime :: FromJSON a => Text -> Result a
      runtime = rule settings "runtime"
      boundedRuntime :: Text -> Result Int
      boundedRuntime key = min timeoutMicros <$> runtime key
  LiveConfig (setting settings "OPENAI_API_KEY")
    <$> runtime "live_host" <*> runtime "live_port" <*> runtime "live_path"
    <*> pure configuration <*> pure opening <*> runtime "setup_micros"
    <*> runtime "write_micros" <*> boundedRuntime "close_micros" <*> boundedRuntime "startup_close_micros"
    <*> runtime "frame_bytes" <*> runtime "frame_micros"
    <*> runtime "input_rate" <*> runtime "provider_rate" <*> runtime "queue_frames"
    <*> runtime "startup_frames" <*> pure limits <*> pure ordinaryMicros
    <*> pure callMicros <*> rule settings "live" "mutation_errors"
    <*> rule settings "live" "background" <*> runtime "websocket_message_bytes"
    <*> rule settings "live" "error_fields"

toolLimits :: Settings -> Result ToolLimits
toolLimits settings = ToolLimits <$> readLimit "MAX_PENDING_RESPONSES"
  <*> readLimit "MAX_RESPONSE_CALLS" <*> readLimit "MAX_TOOL_CALLS" <*> readLimit "MAX_TOOL_TEXT"
  where
    readLimit = rule settings "live"

announcementConfig :: Settings -> Text -> Result LiveConfig
announcementConfig settings line = do
  identity <- voiceIdentity settings
  instructions <- rule settings "protocol" "announcementInstructions"
  rate <- rule settings "runtime" "provider_rate"
  let session = SessionSettings Announcement (setting settings "OPENAI_LIVE_MODEL")
        (setting settings "OPENAI_LIVE_VOICE") (identity <> "\n\n" <> instructions)
        "" "" "" "" 0 [] rate
  liveConfiguration settings session line

handoffAnnouncement :: Settings -> Text -> Result (LiveConfig, Text, Int, Int)
handoffAnnouncement settings line = do
  cfg <- announcementConfig settings line
  let completion = T.unwords (takeLast 5 (T.words line))
  case (rule settings "protocol" "announcementQuietMicros", rule settings "protocol" "announcementMicros") of
    (Right quiet, Right duration) -> Right (cfg, completion, quiet, duration)
    _ -> Left (failure "announcement_configuration_invalid")

takeLast :: Int -> [a] -> [a]
takeLast count values = drop (max 0 (length values - count)) values

telephonyConfiguration :: Settings -> Result TelephonyConfig
telephonyConfiguration settings = TelephonyConfig
  (setting settings "PHONE_ASSISTANT_AMI_USERNAME") (setting settings "PHONE_ASSISTANT_AMI_PASSWORD")
  <$> booleanSetting settings "PHONE_ASSISTANT_DIRECT_CALLS" <*> routes
  <*> integerSetting settings "PHONE_ASSISTANT_DIRECT_CALL_TIMEOUT"
  <*> rule settings "runtime" "recording_directory"
  <*> rule settings "telephony" "service"
  <*> rule settings "ami" "transport"
  where
    routes = RouteSettings <$> booleanSetting settings "TELEMATICA_OUTBOUND_ENABLED"
      <*> pure True <*> booleanSetting settings "HOME_PHONE_ENABLED"

pinConfiguration :: Settings -> Result Pin.PinConfig
pinConfiguration settings = Pin.PinConfig (setting settings "PHONE_ASSISTANT_PRIVATE_PIN_HASH")
  <$> rule settings "pin" "PROTECTED_PREFIXES" <*> rule settings "pin" "windowMicros"
  <*> rule settings "pin" "minimumDigits" <*> rule settings "pin" "maximumDigits"
  <*> rule settings "pin" "prompt" <*> rule settings "pin" "incorrect"
  <*> rule settings "pin" "verified"

endConfiguration :: Settings -> Result EndConfig
endConfiguration settings = EndConfig <$> readRule "farewellText" <*> readRule "farewellCompletion"
  <*> readRule "farewellMicros"
  where
    readRule :: FromJSON a => Text -> Result a
    readRule = rule settings "protocol"

observerConfiguration :: Settings -> Result NoteObserverConfig
observerConfiguration settings = rule settings "notes" "observer"

hostedTools :: Settings -> [Value]
hostedTools settings
  | booleanSetting settings "PHONE_ASSISTANT_WEB_SEARCH" == Right True = toolDefinitions settings "WEB_SEARCH_TOOLS"
  | otherwise = []
