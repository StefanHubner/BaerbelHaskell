{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Session
  ( ownerSession, playHandoff, telephonyConfiguration, liveConfiguration, announcementConfig, runAnnouncement
  ) where

import Baerbel.AudioMetrics (AudioDelivery(..))
import Baerbel.JSON (jsonOptions)
import GHC.Generics (Generic)
import Baerbel.Config (Settings, setting, rule, booleanSetting, integerSetting, numericSetting, toolDefinitions)
import Baerbel.EndCall (EndConfig(..), EndContext(..), EndEffects(..), executeEndCall, parseEndReason)
import Baerbel.ManagedMedia (Registry, ManagedCall(..), withManaged)
import Baerbel.Telephony (TelephonyConfig(..), TelephonyMedia(..), OwnerControl(..), Handoffs, claimHandoff, openOwner)
import Baerbel.Routing (RouteSettings(..), announcement)
import Baerbel.PhoneNumber (validateNumber)
import Baerbel.Logging (Report, reportOwnerEvent)
import Baerbel.Ringback (withRingback)
import Baerbel.Recording (Archive, closeRecording)
import Baerbel.NoteRuntime (NoteObserverConfig(..), NoteRuntime(..), withNoteRuntime)
import qualified Baerbel.Pin as Pin
import Baerbel.Services (Service(..), Services, withCallServices, deliverOwnerNote, ownerProfileContext, dispatch)
import qualified Baerbel.Tools as Tools
import Baerbel.Server (socketMedia)
import Control.Exception (finally)
import Control.Monad (when, void)
import Data.Time (UTCTime, getCurrentTime)
import Network.Socket (Socket)
import qualified Baerbel.Effects as Effects
import Baerbel.Error (Result, failure)
import Baerbel.Live (LiveConfig(..), Media(..), LiveControls(..), ToolLimits(..), runLiveSession, appendTranscript)
import Baerbel.Protocol (SessionRole(..), SessionSettings(..), sessionConfig, voiceIdentity, greetingCommand, buildSessionWithProfile, announcementReady)
import Baerbel.Speech (audiblePCM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Data.Aeson (Value, FromJSON(..), genericParseJSON, object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.List (sortOn, elemIndex)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

liveConfiguration :: Settings -> SessionSettings -> Text -> Result LiveConfig
liveConfiguration settings session greeting = do
  template <- rule settings "protocol" "greetingPrefix"
  limits <- toolLimits settings
  seconds <- numericSetting settings "OPENAI_TIMEOUT_SECONDS"
  toolMicros <- rule settings "runtime" "tool_micros"
  directSeconds <- integerSetting settings "PHONE_ASSISTANT_DIRECT_CALL_TIMEOUT"
  extra <- rule settings "runtime" "direct_extra_micros"
  let callMicros = directSeconds * 1000000 + extra
      ordinaryMicros = min toolMicros (ceiling (seconds * 1000000))
  let configuration = sessionConfig session
      opening = greetingCommand template greeting
      runtime :: FromJSON a => Text -> Result a
      runtime = rule settings "runtime"
  LiveConfig (setting settings "OPENAI_API_KEY")
    <$> runtime "live_host" <*> runtime "live_port" <*> runtime "live_path"
    <*> pure configuration <*> pure opening <*> runtime "setup_micros"
    <*> runtime "write_micros" <*> runtime "frame_bytes" <*> runtime "frame_micros"
    <*> runtime "input_rate" <*> runtime "provider_rate" <*> runtime "queue_frames"
    <*> runtime "startup_frames" <*> pure limits <*> pure ordinaryMicros
    <*> runtime "voice_test_micros" <*> pure callMicros <*> rule settings "live" "mutation_errors"
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
        "" "" "" 0 [] rate
  liveConfiguration settings session line

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

ownerSession :: Settings -> Services -> Report -> Registry -> Handoffs -> Text -> Socket -> IO (Result ())
ownerSession settings factories report registry handoffs identifier connection =
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
      in startOwner settings factories native identifier managed {managedMedia = transport}

playHandoff :: Settings -> Socket -> Text -> IO (Result ())
playHandoff settings connection line = Effects.continue play (rule settings "runtime" "socket")
  where
    play rules = do
      let transport = socketMedia rules connection (const (pure ())) (const (pure ()))
          configured = handoffAnnouncement settings line
          run (cfg, completion, quiet, duration) = boundedAnnouncement duration cfg transport completion quiet
      finally (Effects.continue run configured) (void (finishCall transport))

handoffAnnouncement :: Settings -> Text -> Result (LiveConfig, Text, Int, Int)
handoffAnnouncement settings line = do
  cfg <- announcementConfig settings line
  let completion = T.unwords (takeLast 5 (T.words line))
  case (rule settings "protocol" "announcementQuietMicros", rule settings "protocol" "announcementMicros") of
    (Right quiet, Right duration) -> Right (cfg, completion, quiet, duration)
    _ -> Left (failure "announcement_configuration_invalid")

boundedAnnouncement :: Int -> LiveConfig -> Media -> Text -> Int -> IO (Result ())
boundedAnnouncement duration cfg transport completion quiet =
  Effects.within (failure "announcement_timeout") duration (runAnnouncement cfg transport completion quiet)

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

endConfiguration :: Settings -> Result EndConfig
endConfiguration settings = EndConfig <$> readRule "farewellText" <*> readRule "farewellCompletion"
  <*> readRule "farewellMicros"
  where
    readRule :: FromJSON a => Text -> Result a
    readRule = rule settings "protocol"

startOwner :: Settings -> Services -> TelephonyConfig -> Text -> ManagedCall -> IO (Result ())
startOwner settings factories native identifier managed = case rule settings "startup_ringback" "playback" of
  Left err -> pure (Left err)
  Right rules -> withRingback rules (managedMedia managed) $ \transport ->
    openOwnerSession settings factories native identifier managed {managedMedia = transport}

openOwnerSession :: Settings -> Services -> TelephonyConfig -> Text -> ManagedCall -> IO (Result ())
openOwnerSession settings factories native identifier managed = case pinConfiguration settings of
  Left err -> pure (Left err)
  Right cfg -> do
    gate <- Pin.newPinGate cfg
    Effects.continue (\pin -> finally (bindOwner settings factories native identifier managed pin) (Pin.closePin pin)) gate

bindOwner :: Settings -> Services -> TelephonyConfig -> Text -> ManagedCall -> Pin.PinGate -> IO (Result ())
bindOwner settings factories native identifier managed pin = do
  setKeypad managed (Pin.feedDigit pin)
  bind (directEnabled native)
  where
    bind False = runOwner settings factories managed pin Nothing
    bind True = do
      let hooks = nativeMedia settings managed pin
      opened <- openOwner native validateNumber hooks identifier
      Effects.continue runBound opened
    runBound owner = do
      setReattachAllowed managed (ownerCanResume owner)
      finally (runOwner settings factories managed pin (Just owner)) (closeOwner owner)

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

observerConfiguration :: Settings -> Result NoteObserverConfig
observerConfiguration settings = rule settings "notes" "observer"

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
    voice = [Service "voice" (toolDefinitions settings "VOICE_TEST_TOOLS") (\_ _ _ -> voiceTest settings managed notesRuntime) | isJust owner]
    rank service = fromMaybe (length order) (elemIndex (servicePrefix service) order)
    integrated = sortOn rank (services ++ notesService)
    available = integrated ++ calling ++ voice ++ [end]
    helpSchemas = toolDefinitions settings "HELP_TOOLS"
    schemas = hostedTools settings ++ concatMap serviceTools available ++ helpSchemas

hostedTools :: Settings -> [Value]
hostedTools settings
  | booleanSetting settings "PHONE_ASSISTANT_WEB_SEARCH" == Right True = toolDefinitions settings "WEB_SEARCH_TOOLS"
  | otherwise = []

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


data VoiceSample = VoiceSample
  { sampleVoice :: Text, sampleLabel :: Text, samplePreset :: Text
  , sampleText :: Text, sampleInstructions :: Text
  } deriving (Eq, Show, Generic)
instance FromJSON VoiceSample where parseJSON = genericParseJSON (jsonOptions 6)

voiceTest :: Settings -> ManagedCall -> Maybe NoteRuntime -> IO (Result Value)
voiceTest settings managed notesRuntime = do
  paused <- managedPaused managed
  pending <- maybe (pure False) notePending notesRuntime
  case (paused, pending, voiceSamples settings) of
    (True, _, _) -> pure (Left (failure "voice_test_unavailable"))
    (_, True, _) -> pure (Left (failure "finish_or_cancel_note_before_voice_test"))
    (_, _, Left err) -> pure (Left err)
    (_, _, Right samples) -> do
      played <- Effects.runAll (playSample settings managed) samples
      pure (sampleResult settings samples played)

voiceSamples :: Settings -> Result [VoiceSample]
voiceSamples settings = do
  model <- rule settings "voice_audition" "MODEL"
  samples model
  where
    samples model
      | setting settings "OPENAI_LIVE_MODEL" == model = rule settings "voice_audition" "samples"
      | otherwise = Left (failure "voice_test_requires_current_live_model")

sampleResult :: Settings -> [VoiceSample] -> Result () -> Result Value
sampleResult settings samples result = result >> Right output
  where
    output = object ["status" .= ("ok" :: Text), "played" .= map sampleLabel samples,
      "model" .= setting settings "OPENAI_LIVE_MODEL", "mode" .= ("live" :: Text), "preference_saved" .= False]

playSample :: Settings -> ManagedCall -> VoiceSample -> IO (Result ())
playSample settings managed sample = case sampleConfiguration settings sample of
  Left err -> pure (Left err)
  Right (cfg, quiet, duration, gap) -> do
    controls <- managedControls managed
    let completion = T.unwords (takeLast 5 (T.words (sampleText sample)))
        play current = playAnnouncement current cfg completion quiet False
    let announce = maybe (pure (Left (failure "voice_test_unavailable"))) play controls
    result <- Effects.within (failure "voice_test_timeout") duration announce
    threadDelay gap
    pure (void result)

sampleConfiguration :: Settings -> VoiceSample -> Result (LiveConfig, Int, Int, Int)
sampleConfiguration settings sample = do
  instructions <- rule settings "voice_audition" "sampleInstructions"
  rate <- rule settings "runtime" "provider_rate"
  let content = instructions <> sampleInstructions sample <> "\nLine: " <> sampleText sample
      session = SessionSettings Announcement (setting settings "OPENAI_LIVE_MODEL")
        (sampleVoice sample) content "" "" "" 0 [] rate
  cfg <- liveConfiguration settings session (sampleText sample)
  (,,,) cfg <$> rule settings "voice_audition" "sampleQuietMicros"
    <*> rule settings "voice_audition" "sampleMicros" <*> rule settings "voice_audition" "sampleGapMicros"
