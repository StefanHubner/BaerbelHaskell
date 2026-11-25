{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Session
  ( ownerSession, playHandoff, telephonyConfiguration, liveConfiguration, announcementConfig, runAnnouncement
  ) where

import Baerbel.Core.AudioMetrics ( AudioDelivery(..) )
import Baerbel.Core.Config
    ( Settings, setting, booleanSetting, toolDefinitions, rule )
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.Protocol
    ( buildSessionWithProfile, announcementReady )
import Baerbel.Core.Routing ( announcement, NumberValidator )
import Baerbel.Core.Session
import Baerbel.Runtime.EndCall
    ( EndConfig(..),
      EndContext(..),
      EndEffects(..),
      executeEndCall,
      parseEndReason )
import Baerbel.Runtime.Live
    ( LiveConfig(liveFrameMicros),
      appendTranscript,
      LiveControls(playAnnouncement, hasPendingTools, stopSession),
      Media(observeEvent, finishCall, beforeTool, observeInputAudio,
            afterTool, progressSuppressed, sendAudio, muteInput),
      runLiveSession )
import Baerbel.Runtime.Logging ( Report, reportOwnerEvent )
import Baerbel.Runtime.ManagedMedia
    ( Registry, ManagedCall(..), withManaged )
import Baerbel.Runtime.NoteRuntime
    ( NoteRuntime(notePending, noteExecute, noteEvent, noteAudio),
      withNoteRuntime )
import Baerbel.Runtime.Recording ( Archive, closeRecording )
import Baerbel.Runtime.Ringback ( withRingback )
import Baerbel.Runtime.Server ( socketMedia )
import Baerbel.Runtime.Services
    ( Service(..),
      Services,
      withCallServices,
      deliverOwnerNote,
      ownerProfileContext,
      dispatch )
import Baerbel.Runtime.Speech ( audiblePCM )
import Baerbel.Runtime.Telephony
    ( TelephonyConfig(..),
      TelephonyMedia(..),
      OwnerControl(..),
      Handoffs,
      claimHandoff,
      openOwner )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, TVar, newTVarIO, readTVarIO, writeTVar, modifyTVar' )
import Control.Exception ( finally )
import Control.Monad ( when, void )
import Data.Aeson ( Value )
import Data.List ( sortOn, elemIndex )
import Data.Map.Strict ( Map )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime, getCurrentTime )
import Data.Word ( Word64 )
import GHC.Clock ( getMonotonicTimeNSec )
import Network.Socket ( Socket )
import qualified Data.ByteString as BS
import qualified Baerbel.Runtime.Effects as Effects
import qualified Baerbel.Runtime.Pin as Pin
import qualified Data.Text as T
import qualified Baerbel.Core.Tools as Tools

runAnnouncement :: LiveConfig -> Media -> Text -> Int -> IO (Result ())
runAnnouncement cfg transport completion quietMicros = do
  transcript <- newTVarIO ""
  lastAudio <- newTVarIO Nothing
  let observed = transport
        { observeEvent = mapM_ (atomically . modifyTVar' transcript) . appendTranscript
        , sendAudio = observeAudio (liveFrameMicros cfg) transport lastAudio
        , muteInput = pure True
        }
      session = runLiveSession cfg unavailable observed
      completed = awaitAnnouncement transcript lastAudio completion quietMicros
      unavailable _ _ _ = pure (Left (failure "announcement_has_no_tools"))
  result <- race session completed
  pure $ case result of
    Left (Right ()) -> Left (failure "announcement_incomplete")
    Left (Left err) -> Left err
    Right completionResult -> completionResult

observeAudio :: Int -> Media -> TVar (Maybe Word64) -> BS.ByteString -> IO (Result AudioDelivery)
observeAudio frameMicros transport lastAudio bytes = do
  sent <- sendAudio transport bytes
  now <- getMonotonicTimeNSec
  let speechEnd = now + fromIntegral frameMicros * 1000
  when (sent == Right Delivered && audiblePCM bytes) (atomically (writeTVar lastAudio (Just speechEnd)))
  pure sent

awaitAnnouncement :: TVar Text -> TVar (Maybe Word64) -> Text -> Int -> IO (Result ())
awaitAnnouncement transcript lastAudio completion quietMicros = do
  now <- getMonotonicTimeNSec
  heard <- readTVarIO transcript
  lastSpeech <- readTVarIO lastAudio
  let complete = announcementReady False completion quietMicros now heard lastSpeech
  continue complete
  where
    continue True = pure (Right ())
    continue False = threadDelay 20000 >> awaitAnnouncement transcript lastAudio completion quietMicros

ownerSession :: Settings -> NumberValidator -> Services -> Report -> Registry -> Handoffs -> Text -> Socket -> IO (Result ())
ownerSession settings validate factories report registry handoffs identifier connection =
  Effects.continue route (telephonyConfiguration settings)
  where
    route native = do
      claimed <- claimHandoff native handoffs identifier
      case claimed of
        Left err -> pure (Left err)
        Right (Just line) -> playHandoff settings connection line
        Right Nothing -> withManaged registry identifier connection (startObserved native)
    startObserved native managed =
      let transport = (managedMedia managed) {observeEvent = reportOwnerEvent report identifier}
      in startOwner settings validate factories native identifier managed {managedMedia = transport}

playHandoff :: Settings -> Socket -> Text -> IO (Result ())
playHandoff settings connection line = Effects.continue play (rule settings "runtime" "socket")
  where
    play rules = do
      let transport = socketMedia rules connection (const (pure ())) (const (pure ()))
          configured = handoffAnnouncement settings line
          run (cfg, completion, quiet, duration) = boundedAnnouncement duration cfg transport completion quiet
      finally (Effects.continue run configured) (void (finishCall transport))

boundedAnnouncement :: Int -> LiveConfig -> Media -> Text -> Int -> IO (Result ())
boundedAnnouncement duration cfg transport completion quiet =
  Effects.within (failure "announcement_timeout") duration (runAnnouncement cfg transport completion quiet)

startOwner :: Settings -> NumberValidator -> Services -> TelephonyConfig -> Text -> ManagedCall -> IO (Result ())
startOwner settings validate factories native identifier managed = case rule settings "startup_ringback" "playback" of
  Left err -> pure (Left err)
  Right rules -> withRingback rules (managedMedia managed) $ \transport ->
    openOwnerSession settings validate factories native identifier managed {managedMedia = transport}

openOwnerSession :: Settings -> NumberValidator -> Services -> TelephonyConfig -> Text -> ManagedCall -> IO (Result ())
openOwnerSession settings validate factories native identifier managed = case pinConfiguration settings of
  Left err -> pure (Left err)
  Right cfg -> do
    gate <- Pin.newPinGate cfg
    Effects.continue (\pin -> finally (bindOwner settings validate factories native identifier managed pin) (Pin.closePin pin)) gate

bindOwner :: Settings -> NumberValidator -> Services -> TelephonyConfig -> Text -> ManagedCall -> Pin.PinGate -> IO (Result ())
bindOwner settings validate factories native identifier managed pin = do
  setKeypad managed (Pin.feedDigit pin)
  bind nativeRequired
  where
    nativeRequired = directEnabled native || not (T.null (setting settings "PHONE_ASSISTANT_PRIVATE_PIN_HASH"))
    bind False = runOwner settings factories managed pin Nothing
    bind True = do
      let hooks = nativeMedia settings managed pin
      opened <- openOwner native validate hooks identifier
      Effects.continue runBound opened
    runBound owner = finally (runBoundSession owner) (closeOwner owner)
    runBoundSession owner
      | directEnabled native = do
          setReattachAllowed managed (ownerCanResume owner)
          runOwner settings factories managed pin (Just owner)
      | otherwise = runOwner settings factories managed pin Nothing

ownerCanResume :: OwnerControl -> IO Bool
ownerCanResume owner = do
  answered <- ownerAnswered owner
  ended <- ownerIsEnded owner
  pure (not (answered || ended))

nativeMedia :: Settings -> ManagedCall -> Pin.PinGate -> TelephonyMedia
nativeMedia settings managed pin = TelephonyMedia
  { announceRoute = \route label -> announceManaged settings managed (announcement route label) "I will connect you now" True
  , pauseMedia = pauseManaged managed, resumeExisting = resumeManaged managed
  , waitResumed = waitManaged managed, keypadDigit = Pin.feedDigit pin
  }

announceManaged :: Settings -> ManagedCall -> Text -> Text -> Bool -> IO (Result Bool)
announceManaged settings managed line completion interruptible = case configuration of
  Left err -> pure (Left err)
  Right (cfg, quiet, duration) -> do
    controls <- managedControls managed
    maybe (pure (Left (failure "announcement_unavailable"))) (play cfg quiet duration) controls
  where
    configuration = do
      cfg <- announcementConfig settings line
      let limits = (,) <$> rule settings "protocol" "ownerAnnouncementQuietMicros"
            <*> rule settings "protocol" "ownerAnnouncementMicros"
      (quiet, duration) <- either (const (Left (failure "announcement_unavailable"))) Right limits
      pure (cfg, quiet, duration)
    play cfg quiet duration current = Effects.within (failure "announcement_timeout") duration
      (playAnnouncement current cfg completion quiet interruptible)

runOwner :: Settings -> Services -> ManagedCall -> Pin.PinGate -> Maybe OwnerControl -> IO (Result ())
runOwner settings factories managed pin owner = withCallServices factories $ \services -> do
  context <- fromMaybe (pure (Right "")) (ownerProfileContext factories)
  archive <- openArchive settings owner
  let run text = withNotes settings factories archive (observeEvent (managedMedia managed)) (continueOwner settings services managed pin owner text)
  finally (Effects.continue run context) (maybe (pure ()) closeRecording archive)

openArchive :: Settings -> Maybe OwnerControl -> IO (Maybe Archive)
openArchive settings owner
  | booleanSetting settings "PHONE_ASSISTANT_OWNER_NOTES" /= Right True = pure Nothing
  | otherwise = case owner of
      Nothing -> pure Nothing
      Just controller -> either (const Nothing) Just <$> ownerRecording controller

withNotes :: Settings -> Services -> Maybe Archive -> (Value -> IO ()) -> (Maybe NoteRuntime -> IO (Result ())) -> IO (Result ())
withNotes settings factories archive report action
  | booleanSetting settings "PHONE_ASSISTANT_OWNER_NOTES" /= Right True = action Nothing
  | otherwise = case deliverOwnerNote factories of
      Nothing -> pure (Left (failure "owner_note_mail_unavailable"))
      Just prepare -> Effects.continue (open prepare) configuration
  where
    configuration = (,) <$> observerConfiguration settings <*> rule settings "notes" "selection"
    open prepare (cfg, rules) = do
      deliver <- prepare
      withNoteRuntime cfg rules archive deliver report (action . Just)

continueOwner :: Settings -> [Service] -> ManagedCall -> Pin.PinGate -> Maybe OwnerControl -> Text -> Maybe NoteRuntime -> IO (Result ())
continueOwner settings services managed pin owner profile notesRuntime = do
  now <- getCurrentTime
  let configured = ownerConfiguration settings services managed pin owner profile notesRuntime now
      run (cfg, handlers) = runOwnerMedia cfg handlers managed pin owner notesRuntime
  Effects.continue run configured

ownerConfiguration :: Settings -> [Service] -> ManagedCall -> Pin.PinGate -> Maybe OwnerControl -> Text -> Maybe NoteRuntime -> UTCTime -> Result (LiveConfig, [Service])
ownerConfiguration settings services managed pin owner profile notesRuntime now = do
  ending <- endConfiguration settings
  descriptions <- rule settings "providers" "help"
  order <- rule settings "protocol" "integrationOrder"
  handlers <- ownerServices settings services managed pin owner notesRuntime ending descriptions order
  let schemas = hostedTools settings ++ concatMap serviceTools handlers
  session <- buildSessionWithProfile settings now profile schemas
  cfg <- liveConfiguration settings session (setting settings "PHONE_ASSISTANT_GREETING")
  pure (cfg, handlers)

ownerServices :: Settings -> [Service] -> ManagedCall -> Pin.PinGate -> Maybe OwnerControl -> Maybe NoteRuntime -> EndConfig -> Map Text Text -> [Text] -> Result [Service]
ownerServices settings services managed pin owner notesRuntime ending descriptions order = do
  value <- Tools.help descriptions schemas
  let helpService = Service "assistant" helpSchemas (\_ _ _ -> pure (Right value))
  Right (available ++ [helpService])
  where
    calling = maybe [] (\control -> [Service "direct" (toolDefinitions settings "DIRECT_CALL_TOOLS") (executeDirect control)]) owner
    notesService = maybe [] (\notes -> [Service "owner_note" (toolDefinitions settings "OWNER_NOTE_TOOLS") (noteExecute notes)]) notesRuntime
    end = Service "end" (toolDefinitions settings "END_CALL_TOOLS") (executeOwnerEnd managed pin notesRuntime ending settings)
    rank service = fromMaybe (length order) (elemIndex (servicePrefix service) order)
    integrated = sortOn rank (services ++ notesService)
    available = integrated ++ calling ++ [end]
    helpSchemas = toolDefinitions settings "HELP_TOOLS"
    schemas = hostedTools settings ++ concatMap serviceTools available ++ helpSchemas

executeOwnerEnd :: ManagedCall -> Pin.PinGate -> Maybe NoteRuntime -> EndConfig -> Settings -> Text -> Value -> Text -> IO (Result Value)
executeOwnerEnd managed pin notesRuntime cfg settings _ args requestId = do
  controls <- managedControls managed
  case controls of
    Nothing -> pure (Left (failure "end_call_unavailable"))
    Just current -> Effects.continue (run current) (parseEndReason args)
  where
    run current = executeEndCall cfg (endSnapshot managed pin notesRuntime current requestId) (endEffects managed current settings)

endSnapshot :: ManagedCall -> Pin.PinGate -> Maybe NoteRuntime -> LiveControls -> Text -> IO EndContext
endSnapshot managed pin notesRuntime controls requestId = EndContext True
  <$> managedPaused managed <*> Pin.collecting pin <*> maybe (pure False) notePending notesRuntime
  <*> hasPendingTools controls requestId

endEffects :: ManagedCall -> LiveControls -> Settings -> EndEffects
endEffects managed controls settings = EndEffects
  { announceFarewell = \line completion -> announceManaged settings managed line completion True
  , terminateCall = finishCall (managedMedia managed)
  , markEnded = stopSession controls
  }

runOwnerMedia :: LiveConfig -> [Service] -> ManagedCall -> Pin.PinGate -> Maybe OwnerControl -> Maybe NoteRuntime -> IO (Result ())
runOwnerMedia cfg services managed pin owner notesRuntime = do
  let transport = (managedMedia managed)
        { muteInput = Pin.collecting pin, beforeTool = Pin.prepareTool pin
        , observeEvent = \event -> observeEvent (managedMedia managed) event >> maybe (pure ()) (`noteEvent` event) notesRuntime
        , observeInputAudio = maybe (const (pure ())) noteAudio notesRuntime
        , afterTool = afterOwnerTool managed owner
        , progressSuppressed = (||) <$> Pin.collecting pin <*> maybe (pure False) notePending notesRuntime
        }
  result <- runLiveSession cfg (dispatch services) transport
  answered <- maybe (pure False) ownerAnswered owner
  finish answered result
  where
    finish True result = maybe (pure result) waitOwnerFinished owner
    finish False result = pure result

afterOwnerTool :: ManagedCall -> Maybe OwnerControl -> Text -> Value -> IO ()
afterOwnerTool managed owner name _
  | name /= "direct_call" = pure ()
  | otherwise = do
      answered <- maybe (pure False) ownerAnswered owner
      controls <- managedControls managed
      when answered (maybe (pure ()) stopSession controls)
