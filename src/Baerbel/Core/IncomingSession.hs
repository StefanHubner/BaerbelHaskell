{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.IncomingSession where

import Baerbel.Core.Config (Settings, setting, rule, toolDefinitions)
import Baerbel.Core.Error (Result)
import Baerbel.Core.Incoming (Decision(..), restrictTools, callerDisplayName)
import Baerbel.Core.Live (LiveConfig)
import Baerbel.Core.Protocol (SessionRole(Restricted), SessionSettings(..), voiceIdentity, applyInstructions)
import Baerbel.Core.Session (liveConfiguration, announcementConfig)
import Baerbel.Core.SMS (Delivery(..))
import Baerbel.Core.Telephony (IncomingEnd(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

screenBudget :: Settings -> Bool -> Double -> Double -> Result Int
screenBudget settings allowMessages seconds elapsed = do
  announcementSeconds <- rule settings "incoming" "ANNOUNCEMENT_TIMEOUT"
  reserve <- rule settings "incoming" reserveKey
  maximumMicros <- rule settings "incoming" "screeningMicros"
  let remaining = seconds * 1000000 - elapsed - announcementSeconds * 1000000 - reserve
  pure (max 0 (min maximumMicros (floor remaining)))
  where
    reserveKey :: Text
    reserveKey | allowMessages = "messageWaitMicros"
               | otherwise = "reserveMicros"

messageCompletion :: Delivery -> (Text, IncomingEnd)
messageCompletion Sent = ("messageSent", MessageSent)
messageCompletion Failed = ("messageUncertain", MessageFailed)
messageCompletion _ = ("messageUncertain", MessageUncertain)

announcement :: Settings -> Text -> [(Text, Text)] -> Result (LiveConfig, Text, Int, Int)
announcement settings key replacements = do
  template <- rule settings "incoming" key
  fields <- names settings
  let line = applyInstructions template (replacements ++ fields)
      wordsInLine = T.words line
      completion = T.unwords (drop (max 0 (length wordsInLine - 5)) wordsInLine)
  cfg <- announcementConfig settings line
  quiet <- rule settings "protocol" "announcementQuietMicros"
  seconds <- rule settings "incoming" "ANNOUNCEMENT_TIMEOUT"
  let duration = seconds * 1000000
  pure (cfg, completion, quiet, duration)

names :: Settings -> Result [(Text, Text)]
names settings = do
  assistant <- spokenName settings
  fallback <- rule settings "settings" "incoming_owner_name"
  let configured = setting settings "INCOMING_OWNER_NAME"
      requested | T.null configured = fallback
                | otherwise = configured
      owner = fromMaybe fallback (callerDisplayName False requested)
  pure [("{owner}", owner), ("{assistant}", assistant)]

spokenName :: Settings -> Result Text
spokenName settings = rule settings "incoming" "spokenName"

restrictedSettings :: Settings -> Bool -> Bool -> Result LiveConfig
restrictedSettings settings allowMessages messageOnly = do
  identity <- voiceIdentity settings
  fields <- names settings
  template <- rule settings "incoming" (instructionKey allowMessages messageOnly)
  greetingTemplate <- rule settings "incoming" greetingKey
  tokens <- rule settings "incoming" tokenKey
  rate <- rule settings "runtime" "provider_rate"
  let instructions = applyInstructions template (("{identity}", identity) : fields)
      greeting = applyInstructions greetingTemplate fields
      schemas = toolDefinitions settings "SCREENING_TOOLS" ++ toolDefinitions settings "MESSAGE_TOOL"
      tools = restrictTools allowMessages messageOnly schemas
      session = SessionSettings Restricted (setting settings "OPENAI_LIVE_MODEL")
        (setting settings "OPENAI_LIVE_VOICE") instructions (setting settings "OPENAI_CHAT_MODEL")
        instructions (setting settings "OPENAI_CHAT_REASONING_EFFORT")
        (setting settings "OPENAI_CHAT_SERVICE_TIER") tokens tools rate
  liveConfiguration settings session greeting
  where
    greetingKey :: Text
    greetingKey | messageOnly = "messageGreeting"
                | otherwise = "screenGreeting"
    tokenKey :: Text
    tokenKey | allowMessages = "messageTokenLimit"
             | otherwise = "screenTokenLimit"

instructionKey :: Bool -> Bool -> Text
instructionKey False False = "screenWithoutMessages"
instructionKey True False = "screenWithMessages"
instructionKey False True = "messageWithoutMessages"
instructionKey True True = "messageWithMessages"

decisionLabels :: Decision -> (Text, Text)
decisionLabels (Forward reason _ _) = ("forward", reason)
decisionLabels (Decline reason _ _) = ("decline", reason)
decisionLabels (LeaveMessage _ _) = ("message", "message")
