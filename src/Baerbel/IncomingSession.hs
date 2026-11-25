{-# LANGUAGE OverloadedStrings #-}
module Baerbel.IncomingSession
  ( IncomingServices(..), incomingSession, restrictedSettings, spokenName
  ) where

import Baerbel.AMI (Channel(..))
import Baerbel.Config (Settings, setting, rule, toolDefinitions, booleanSetting)
import Baerbel.Error (Result, failure)
import Baerbel.Incoming (Decision(..), evaluateDecision, restrictTools, handoffLine, callerDisplayName)
import Baerbel.Logging (Report, reportIncomingEvent)
import Baerbel.JSON (resultValue)
import Baerbel.Live (LiveConfig, Media(..), runLiveSession)
import Baerbel.ManagedMedia (Registry, ManagedCall(..), withManaged)
import Baerbel.Messages (Messages(..), Context)
import Baerbel.Protocol (SessionRole(Restricted), SessionSettings(..), voiceIdentity, applyInstructions)
import Baerbel.Provider (Execute)
import Baerbel.Session (liveConfiguration, announcementConfig, runAnnouncement, telephonyConfiguration, playHandoff)
import Baerbel.SMS (Receipt(..), Delivery(..))
import Baerbel.Telephony (TelephonyConfig, IncomingControl(..), IncomingEnd(..), Handoffs, claimHandoff, openIncoming)
import Control.Concurrent.Async (Async, async, race, wait)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (void)
import Data.Aeson (Value(..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Socket)
import System.Timeout (timeout)

data IncomingServices = IncomingServices
  { lookupIncomingCaller :: Text -> IO (Maybe Text)
  , incomingMessages :: Maybe Messages }

data IncomingScope = IncomingScope
  { scopeSettings :: Settings, scopeServices :: IncomingServices
  , scopeIdentifier :: Text, scopeNative :: IncomingControl
  , scopeMedia :: Media, scopeContext :: Maybe Context, scopeAllowsMessages :: Bool, scopeReport :: Report }

data Selected = Selected Decision (Maybe (Async Receipt))

incomingSession :: Settings -> IncomingServices -> Report -> Registry -> Handoffs -> Text -> Socket -> IO (Result ())
incomingSession settings services output registry handoffs identifier connection =
  either (pure . Left) route (telephonyConfiguration settings)
  where
    route :: TelephonyConfig -> IO (Result ())
    route native = do
      claimed <- claimHandoff native handoffs identifier
      let screen = withManaged registry identifier connection (bindSession settings services output native handoffs identifier)
      either (pure . Left) (maybe screen (playHandoff settings connection)) claimed

bindSession :: Settings -> IncomingServices -> Report -> TelephonyConfig -> Handoffs -> Text -> ManagedCall -> IO (Result ())
bindSession settings services output native handoffs identifier managed = do
  opened <- openIncoming native handoffs identifier (pauseManaged managed)
  either (pure . Left) run opened
  where
    run :: IncomingControl -> IO (Result ())
    run control = boundSession settings services output identifier control (managedMedia managed) `finally` closeIncoming control

boundSession :: Settings -> IncomingServices -> Report -> Text -> IncomingControl -> Media -> IO (Result ())
boundSession settings services output identifier native transport = do
  context <- maybe (pure Nothing) (\service -> beginContext service (eventIdentifier native)) (incomingMessages services)
  let allows = isJust (incomingMessages services) && incomingMessagesEnabled native
        && booleanSetting settings "INCOMING_MESSAGES_ENABLED" == Right True
      scope = IncomingScope settings services identifier native transport context allows output
  boundedWorkflow scope `finally` finishScope scope

finishScope :: IncomingScope -> IO ()
finishScope scope = withContext scope finishContext

eventIdentifier :: IncomingControl -> Text
eventIdentifier = channelUniqueId . incomingNativeChannel

boundedWorkflow :: IncomingScope -> IO (Result ())
boundedWorkflow scope = do
  let micros = floor (min 60 (incomingSeconds (scopeNative scope)) * 1000000)
  result <- timeout micros (workflow scope)
  maybe (terminateIncoming (scopeNative scope) ScreeningTimeout) pure result

workflow :: IncomingScope -> IO (Result ())
workflow scope = do
  started <- getMonotonicTimeNSec
  name <- lookupKnown scope
  let messageOnly = channelMessageOnly (incomingNativeChannel (scopeNative scope))
  case (messageOnly && not (scopeAllowsMessages scope), name) of
    (True, _) -> speak scope "unavailableMessage" [] >> terminateIncoming (scopeNative scope) MessageFailed
    (False, Just known) -> forwardKnown scope known
    (False, Nothing) -> screenUnknown scope started

lookupKnown :: IncomingScope -> IO (Maybe Text)
lookupKnown scope
  | channelMessageOnly (incomingNativeChannel native) = pure Nothing
  | otherwise = maybe (pure Nothing) lookupNumber (incomingCallback native)
  where
    native :: IncomingControl
    native = scopeNative scope
    lookupNumber :: Text -> IO (Maybe Text)
    lookupNumber number = case rule (scopeSettings scope) "incoming" "lookupMicros" of
      Left _ -> pure Nothing
      Right micros -> fmap (joinName . fromMaybe Nothing) (timeout micros (lookupIncomingCaller (scopeServices scope) number))
    joinName :: Maybe Text -> Maybe Text
    joinName candidate = candidate >>= callerDisplayName False

forwardKnown :: IncomingScope -> Text -> IO (Result ())
forwardKnown scope name = do
  let short = fromMaybe name (callerDisplayName True name)
  observeKnown scope short
  scopeReport scope "incoming_classified" ["call_id" .= scopeIdentifier scope, "caller_class" .= ("known" :: Text)]
  speak scope "knownGreeting" [("{name}", short)]
  forwardWithFacts scope name "" True

observeKnown :: IncomingScope -> Text -> IO ()
observeKnown scope name = withContext scope (\service context -> knownContext service context name)

screenUnknown :: IncomingScope -> Word64 -> IO (Result ())
screenUnknown scope started = do
  now <- getMonotonicTimeNSec
  let callerClass | channelMessageOnly (incomingNativeChannel (scopeNative scope)) = "message"
                  | otherwise = "unknown"
  scopeReport scope "incoming_classified" ["call_id" .= scopeIdentifier scope, "caller_class" .= (callerClass :: Text)]
  let elapsed = fromIntegral (now - started) / 1000
  case screenBudget scope elapsed of
    Left problem -> pure (Left problem)
    Right micros | micros <= 0 -> terminateIncoming (scopeNative scope) ScreeningTimeout
    Right micros -> decideUnknown scope micros

screenBudget :: IncomingScope -> Double -> Result Int
screenBudget scope elapsed = do
  announcementSeconds <- rule settings "incoming" "ANNOUNCEMENT_TIMEOUT"
  reserve <- rule settings "incoming" reserveKey
  maximumMicros <- rule settings "incoming" "screeningMicros"
  let remaining = incomingSeconds (scopeNative scope) * 1000000 - elapsed - announcementSeconds * 1000000 - reserve
  pure (max 0 (min maximumMicros (floor remaining)))
  where
    settings :: Settings
    settings = scopeSettings scope
    reserveKey :: Text
    reserveKey | scopeAllowsMessages scope = "messageWaitMicros"
               | otherwise = "reserveMicros"

decideUnknown :: IncomingScope -> Int -> IO (Result ())
decideUnknown scope micros = case restrictedSettings (scopeSettings scope) (scopeAllowsMessages scope) messageOnly of
  Left problem -> pure (Left problem)
  Right cfg -> do
    selected <- selectDecision scope micros cfg
    either (pure . Left) (completeDecision scope) selected
  where
    messageOnly :: Bool
    messageOnly = channelMessageOnly (incomingNativeChannel (scopeNative scope))

selectDecision :: IncomingScope -> Int -> LiveConfig -> IO (Result Selected)
selectDecision scope micros cfg = do
  selected <- newEmptyTMVarIO
  lock <- newMVar ()
  let execute :: Execute
      execute name arguments _ = withMVar lock (const (submitDecision scope selected name arguments))
      transport = (scopeMedia scope) {observeEvent = observeIncoming scope}
      live = runLiveSession cfg execute transport
      decision = atomically (readTMVar selected)
  result <- timeout micros (race live decision)
  pure (selectedResult result)

selectedResult :: Maybe (Either (Result ()) Selected) -> Result Selected
selectedResult Nothing = Right (Selected (Decline "timeout" "" "") Nothing)
selectedResult (Just (Right decision)) = Right decision
selectedResult (Just (Left (Left problem))) = Left problem
selectedResult (Just (Left (Right ()))) = Left (failure "incoming_screening_ended_without_decision")

submitDecision :: IncomingScope -> TMVar Selected -> Text -> Value -> IO (Result Value)
submitDecision scope selected name arguments = do
  previous <- atomically (tryReadTMVar selected)
  let prior = fmap (\(Selected decision _) -> decision) previous
      messageOnly = channelMessageOnly (incomingNativeChannel (scopeNative scope))
      evaluated = evaluateDecision (scopeAllowsMessages scope) messageOnly prior name arguments
  either (pure . Left) submit evaluated
  where
    submit :: Decision -> IO (Result Value)
    submit decision = do
      delivery <- startDelivery scope decision
      observeDecision scope decision
      atomically (putTMVar selected (Selected decision delivery))
      pure (Right (resultValue "decision_received"))

startDelivery :: IncomingScope -> Decision -> IO (Maybe (Async Receipt))
startDelivery scope (LeaveMessage name body) = case incomingMessages (scopeServices scope) of
  Nothing -> pure Nothing
  Just service -> Just <$> async (deliverMessage service identifier (scopeIdentifier scope) name body callback)
  where
    identifier :: Text
    identifier = eventIdentifier (scopeNative scope)
    callback :: Maybe Text
    callback = incomingCallback (scopeNative scope)
startDelivery _ _ = pure Nothing

observeDecision :: IncomingScope -> Decision -> IO ()
observeDecision scope decision = withContext scope (\service context -> decideContext service context decision)

observeIncoming :: IncomingScope -> Value -> IO ()
observeIncoming scope event = reportIncomingEvent (scopeReport scope) (scopeIdentifier scope) event >> observeTranscript scope event

observeTranscript :: IncomingScope -> Value -> IO ()
observeTranscript scope (Object fields)
  | KM.lookup "type" fields == Just (String "session.input_transcript.delta") = case KM.lookup "delta" fields of
      Just (String delta) -> withContext scope (\service context -> observeContext service context delta)
      _ -> pure ()
observeTranscript _ _ = pure ()

withContext :: IncomingScope -> (Messages -> Context -> IO ()) -> IO ()
withContext scope action = case (incomingMessages (scopeServices scope), scopeContext scope) of
  (Just service, Just context) -> action service context
  _ -> pure ()

completeDecision :: IncomingScope -> Selected -> IO (Result ())
completeDecision scope (Selected decision delivery) = do
  observeDecision scope decision
  let (selected, reason) = decisionLabels decision
  scopeReport scope "incoming_decision" ["call_id" .= scopeIdentifier scope, "decision" .= selected, "reason" .= reason]
  complete decision
  where
    complete :: Decision -> IO (Result ())
    complete (Forward _ name summary) = speak scope "forwardAnnouncement" [] >> forwardWithFacts scope name summary False
    complete (Decline reason _ _) = do
      speak scope "declineAnnouncement" []
      let outcome | reason == "timeout" = ScreeningTimeout
                  | otherwise = Declined
      terminateIncoming (scopeNative scope) outcome
    complete (LeaveMessage _ _) = finishMessage scope delivery

finishMessage :: IncomingScope -> Maybe (Async Receipt) -> IO (Result ())
finishMessage scope delivery = do
  result <- awaitMessage (scopeSettings scope) delivery
  let status = maybe Uncertain receiptStatus result
      (announcementKey, reason) = messageCompletion status
  scopeReport scope "incoming_message_result" ["call_id" .= scopeIdentifier scope, "status" .= status]
  speak scope announcementKey []
  terminateIncoming (scopeNative scope) reason

messageCompletion :: Delivery -> (Text, IncomingEnd)
messageCompletion Sent = ("messageSent", MessageSent)
messageCompletion Failed = ("messageUncertain", MessageFailed)
messageCompletion _ = ("messageUncertain", MessageUncertain)

awaitMessage :: Settings -> Maybe (Async Receipt) -> IO (Maybe Receipt)
awaitMessage _ Nothing = pure (Just (Receipt Failed Nothing Nothing))
awaitMessage settings (Just task) = case rule settings "incoming" "messageWaitMicros" of
  Left _ -> pure Nothing
  Right micros -> timeout micros (wait task)

forwardWithFacts :: IncomingScope -> Text -> Text -> Bool -> IO (Result ())
forwardWithFacts scope name summary known = case rule (scopeSettings scope) "incoming" "handoff" of
  Left problem -> pure (Left problem)
  Right rules -> forwardIncoming (scopeNative scope) (handoffLine rules name summary known)

speak :: IncomingScope -> Text -> [(Text, Text)] -> IO ()
speak scope key replacements = case announcement scope key replacements of
  Left _ -> pure ()
  Right (cfg, completion, quiet, duration) -> void (timeout duration (runAnnouncement cfg (scopeMedia scope) completion quiet))

announcement :: IncomingScope -> Text -> [(Text, Text)] -> Result (LiveConfig, Text, Int, Int)
announcement scope key replacements = do
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
  where
    settings :: Settings
    settings = scopeSettings scope

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
spokenName settings = do
  active <- rule settings "voice_catalogue" "DEFAULT_ACTIVE_PRESET"
  let selected = setting settings "PHONE_ASSISTANT_VOICE_PRESET" == active && setting settings "OPENAI_LIVE_VOICE" == "willow"
      key | selected = "activeSpokenName"
          | otherwise = "defaultSpokenName"
  rule settings "incoming" key

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
        instructions (setting settings "OPENAI_CHAT_REASONING_EFFORT") tokens tools rate
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
