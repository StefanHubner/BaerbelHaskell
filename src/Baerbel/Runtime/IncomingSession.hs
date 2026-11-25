{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.IncomingSession
  ( IncomingServices(..), incomingSession, restrictedSettings, spokenName
  ) where

import Baerbel.Core.Config ( Settings, booleanSetting, rule )
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.Incoming
    ( Decision(LeaveMessage, Forward, Decline),
      evaluateDecision,
      handoffLine,
      callerDisplayName )
import Baerbel.Core.IncomingSession
import Baerbel.Core.JSON ( resultValue, eventDelta )
import Baerbel.Runtime.AMI ( Channel(..) )
import Baerbel.Runtime.Live
    ( LiveConfig, Media(..), runLiveSession )
import Baerbel.Runtime.Logging ( Report, reportIncomingEvent )
import Baerbel.Runtime.ManagedMedia
    ( Registry, ManagedCall(..), withManaged )
import Baerbel.Runtime.Messages ( Messages(..), Context )
import Baerbel.Runtime.Provider ( Execute )
import Baerbel.Runtime.SMS ( Receipt(..), Delivery(..) )
import Baerbel.Runtime.Session
    ( telephonyConfiguration, runAnnouncement, playHandoff )
import Baerbel.Runtime.Telephony
    ( TelephonyConfig,
      IncomingControl(..),
      IncomingEnd(..),
      Handoffs,
      claimHandoff,
      openIncoming )
import Control.Concurrent.Async ( Async, async, race, wait )
import Control.Concurrent.MVar ( newMVar, withMVar )
import Control.Concurrent.STM
    ( atomically,
      newEmptyTMVarIO,
      putTMVar,
      readTMVar,
      tryReadTMVar,
      TMVar )
import Control.Exception ( finally )
import Control.Monad ( void )
import Data.Aeson ( Value, (.=) )
import Data.Maybe ( fromMaybe, isJust )
import Data.Text ( Text )
import Data.Word ( Word64 )
import GHC.Clock ( getMonotonicTimeNSec )
import Network.Socket ( Socket )
import System.Timeout ( timeout )
import qualified Baerbel.Runtime.Effects as Effects

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
  Effects.continue route (telephonyConfiguration settings)
  where
    route :: TelephonyConfig -> IO (Result ())
    route native = do
      claimed <- claimHandoff native handoffs identifier
      let screen = withManaged registry identifier connection (bindSession settings services output native handoffs identifier)
      Effects.continue (maybe screen (playHandoff settings connection)) claimed

bindSession :: Settings -> IncomingServices -> Report -> TelephonyConfig -> Handoffs -> Text -> ManagedCall -> IO (Result ())
bindSession settings services output native handoffs identifier managed = do
  opened <- openIncoming native handoffs identifier (pauseManaged managed)
  Effects.continue run opened
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
  case screenBudget (scopeSettings scope) (scopeAllowsMessages scope) (incomingSeconds (scopeNative scope)) elapsed of
    Left problem -> pure (Left problem)
    Right micros | micros <= 0 -> terminateIncoming (scopeNative scope) ScreeningTimeout
    Right micros -> decideUnknown scope micros

decideUnknown :: IncomingScope -> Int -> IO (Result ())
decideUnknown scope micros = case restrictedSettings (scopeSettings scope) (scopeAllowsMessages scope) messageOnly of
  Left problem -> pure (Left problem)
  Right cfg -> do
    selected <- selectDecision scope micros cfg
    Effects.continue (completeDecision scope) selected
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
  Effects.continue submit evaluated
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
observeIncoming scope event = do
  reportIncomingEvent (scopeReport scope) (scopeIdentifier scope) event
  let append delta = withContext scope (\service context -> observeContext service context delta)
  maybe (pure ()) append (eventDelta "session.input_transcript.delta" event)

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
speak scope key replacements = case announcement (scopeSettings scope) key replacements of
  Left _ -> pure ()
  Right (cfg, completion, quiet, duration) -> void (timeout duration (runAnnouncement cfg (scopeMedia scope) completion quiet))
