module LiveCheck.Conversation (conversationChecks) where

import Baerbel.Core.Config (Settings(..), booleanSetting, rule, setting, toolDefinitions)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.Incoming (Decision(..), evaluateDecision)
import Baerbel.Core.IncomingSession (restrictedSettings)
import Baerbel.Core.JSON (resultValue, textField)
import Baerbel.Core.Live (LiveConfig(..), advertisedNames)
import Baerbel.Core.Protocol (announcementComplete, buildSession)
import Baerbel.Core.Profile (ProfileState(..))
import Baerbel.Core.Session (announcementConfig, endConfiguration, liveConfiguration, takeLast)
import Baerbel.Runtime.EndCall (EndConfig, EndContext(..), EndEffects(..), executeEndCall, parseEndReason)
import Baerbel.Runtime.Effects (continue, within)
import Baerbel.Runtime.HTTP (RawSend, Send, newRawSender)
import Baerbel.Runtime.Live (LiveControls(..), finishCall)
import Baerbel.Runtime.Profile (ProfileStore(..), profile)
import Baerbel.Runtime.Provider (Execute)
import Baerbel.Runtime.Services (Service(..), dispatch)
import Baerbel.Runtime.Store (profileStore)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, waitCatch, withAsync)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import LiveCheck.Check (CheckResult, check, ensure, skip)
import LiveCheck.Conversation.Audio (spoken, speechPair)
import LiveCheck.Conversation.Pin (pinCheck)
import LiveCheck.Conversation.Model
import LiveCheck.Conversation.Session
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

conversationChecks :: Settings -> Send -> IO [CheckResult]
conversationChecks settings _ = do
  speech <- newRawSender
  sequence
    [ greetingResult settings
    , check "live_owner_schema_acceptance" (schemaCheck settings)
    , check "live_general_and_followup" (generalCheck settings speech)
    , webResult settings speech
    , check "live_temporary_profile_tools" (profileCheck settings speech)
    , check "live_pending_tool_responsiveness" (backgroundCheck settings speech)
    , check "live_announcement_interruption" (interruptionCheck settings speech)
    , check "live_owner_hangup" (hangupCheck settings speech)
    , check "live_restricted_screening" (screeningCheck settings speech)
    , check "live_private_pin_handoff" (pinCheck settings speech)
    , check "live_session_cancellation" (cancellationCheck settings)
    ]

unavailable :: Execute
unavailable _ _ _ = pure (Left (failure "live_check_unexpected_tool"))

schemaCheck :: Settings -> IO (Result Value)
schemaCheck settings = withConversation settings schemas (const unavailable) inspect
  where
    groups = ["PROFILE_TOOLS", "GMAIL_TOOLS", "CALENDAR_TOOLS", "CONTACTS_TOOLS", "MAPS_TOOLS",
      "WINDGURU_TOOLS", "WINDGURU_SET_TOOL", "OWNER_NOTE_TOOLS", "DIRECT_CALL_TOOLS",
      "VOICE_TEST_TOOLS", "HELP_TOOLS", "END_CALL_TOOLS", "WEB_SEARCH_TOOLS"]
    schemas = concatMap (toolDefinitions settings) groups
    inspect running = do
      observed <- await running "live_check_audio_input_timeout" 5000000 (\_ -> seen "check.input.audio")
      pure (object ["accepted_tool_schemas" .= length schemas] <$ observed)

generalCheck :: Settings -> RawSend -> IO (Result Value)
generalCheck settings sender = speechPair settings sender firstText secondText >>= continue run
  where
    firstText = "What is two plus two? Please answer in one short sentence."
    secondText = "Thanks, and now what is three plus three? Please answer in one short sentence."
    run (firstClip, nextClip) = withConversation settings [] (const unavailable) (generalAction firstClip nextClip)

generalAction :: ByteString -> ByteString -> Running -> IO (Result Value)
generalAction firstClip nextClip running = do
  firstAnswer <- answer running firstClip ["four", "4"]
  continue follow firstAnswer
  where
    follow firstTiming = do
      nextAnswer <- answer running nextClip ["six", "6"]
      trace <- snapshot running
      pure (generalResult firstTiming nextAnswer trace)

generalResult :: Value -> Result Value -> Trace -> Result Value
generalResult firstTiming nextAnswer trace = do
  secondTiming <- nextAnswer
  ensure "live_check_general_used_tool" (null (calls trace))
  ensure "live_check_followup_ended_conversation" (not (traceFinished trace))
  Right (object ["first_turn" .= firstTiming, "followup_turn" .= secondTiming])

withProfile :: Settings -> (ProfileStore -> Execute -> IO (Result Value)) -> IO (Result Value)
withProfile settings action = continue open configured
  where
    configured = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
    open (profileRules, storeRules) = withSystemTempDirectory "baerbel-live-profile" $ \directory -> do
      setFileMode directory 0o700
      prepared <- profileStore profileRules storeRules (directory </> "profile.json")
      continue (\store -> action store (profile profileRules store getCurrentTime)) prepared

profileSchemas :: Settings -> [Text] -> [Value]
profileSchemas settings names = filter named (toolDefinitions settings "PROFILE_TOOLS")
  where named schema = textField "name" schema `elem` map Just names

profileCheck :: Settings -> RawSend -> IO (Result Value)
profileCheck settings sender = withProfile settings prepare
  where
    prepare store execute = speechPair settings sender putText getText >>= continue (run store execute)
    run store execute clips = do
      result <- withConversation settings schemas (const (dispatch [Service "profile" schemas execute])) (profileAction clips)
      saved <- profileRead store
      pure (result <* (saved >>= verifyStored))
    schemas = profileSchemas settings ["profile_set", "profile_get"]
    putText = "Please save my profile field synthetic_test_code with the exact value violet badger seven. Use profile_set."
    getText = "Now use profile_get to read synthetic_test_code and tell me its saved value."

profileAction :: (ByteString, ByteString) -> Running -> IO (Result Value)
profileAction (putClip, getClip) running = do
  putEnd <- say running putClip
  written <- await running "live_check_profile_write_timeout" 35000000 (writeComplete putEnd)
  continue readBack written
  where
    readBack _ = do
      getEnd <- say running getClip
      readResult <- await running "live_check_profile_read_timeout" 35000000 (codeSpoken getEnd)
      pure (toJSON . turnTiming getEnd <$> readResult)

writeComplete :: Word64 -> Word64 -> Trace -> Bool
writeComplete since now trace = "profile_set" `elem` calls trace
  && seen "check.tool.submitted" trace && quietAfter since now trace

verifyStored :: ProfileState -> Result ()
verifyStored state = ensure "live_check_profile_value_mismatch"
  (Map.lookup "synthetic_test_code" (profileValues state) == Just "violet badger seven")

codeSpoken :: Word64 -> Word64 -> Trace -> Bool
codeSpoken since now trace = matched && audibleSince since trace && quietAfter since now trace
  where
    content = answerSince since trace
    matched = hasPhrase "violet badger seven" content || hasPhrase "violet badger 7" content

backgroundCheck :: Settings -> RawSend -> IO (Result Value)
backgroundCheck settings sender = withProfile settings prepare
  where
    prepare _ execute = execute "profile_set" seed "live-check-seed" >>= continue (const (clips execute))
    seed = object ["key" .= ("synthetic_test_code" :: Text), "value" .= ("violet badger seven" :: Text)]
    clips execute = speechPair settings sender request checkIn >>= continue (run execute)
    run execute prepared = withConversation settings schemas (const (dispatch [Service "profile" schemas (delayed execute)])) (backgroundAction prepared)
    schemas = profileSchemas settings ["profile_get"]
    request = "Please use profile_get to read my synthetic_test_code and tell me the value. Do not guess it."
    checkIn = "Are you still there? Please answer me while the lookup finishes."

delayed :: Execute -> Execute
delayed execute name arguments ident = threadDelay 20000000 >> execute name arguments ident

backgroundAction :: (ByteString, ByteString) -> Running -> IO (Result Value)
backgroundAction (request, checkIn) running = do
  _ <- say running request
  started <- await running "live_check_tool_not_started" 35000000 (\_ -> seen "check.tool.started")
  continue (const acknowledge) started
  where
    acknowledge = do
      threadDelay 2000000
      end <- say running checkIn
      response <- await running "live_check_pending_tool_unresponsive" 14000000 (pendingAnswer end)
      continue (finish end) response
    finish end acknowledged = do
      completed <- await running "live_check_tool_result_not_spoken" 35000000 (codeSpoken end)
      pure (backgroundResult end acknowledged completed)

pendingAnswer :: Word64 -> Word64 -> Trace -> Bool
pendingAnswer since _ trace = not (seen "check.tool.finished" trace) && audibleSince since trace
  && hasWords ["here", "checking", "working", "looking", "reading", "waiting"] (answerSince since trace)

backgroundResult :: Word64 -> Trace -> Result Trace -> Result Value
backgroundResult since acknowledged completed = do
  trace <- completed
  ensure "live_check_duplicate_pending_tool" (calls trace == ["profile_get"])
  Right (object ["injected_delay_ms" .= (20000 :: Int), "acknowledgement" .= turnTiming since acknowledged,
    "result_spoken_after_completion" .= seen "check.tool.submitted" trace])

interruptionCheck :: Settings -> RawSend -> IO (Result Value)
interruptionCheck settings sender = spoken settings sender text >>= continue run
  where
    text = "Stop please. I have another question. What is two plus two?"
    run clip = withConversation settings [] (const unavailable) (interruptionAction settings clip)

interruptionAction :: Settings -> ByteString -> Running -> IO (Result Value)
interruptionAction settings clip running = do
  installed <- controls running
  continue start ((,) <$> installed <*> configured)
  where
    line = "This is a synthetic announcement. I will describe a peaceful garden with tall trees, colourful flowers, a winding path, and birds singing in the branches. The garden has a small pond beside a wooden bench, where people can sit and enjoy the sunshine. The synthetic announcement is now finished."
    configured = (,) <$> announcementConfig settings line <*> rule settings "protocol" "ownerAnnouncementQuietMicros"
    start (current, (cfg, quiet)) = do
      began <- getMonotonicTimeNSec
      let preview = timePreview (playAnnouncement current cfg "synthetic announcement is now finished" quiet True)
      withAsync preview (interruptPreview running clip began)

interruptPreview :: Running -> ByteString -> Word64 -> Async (Word64, Result Bool) -> IO (Result Value)
interruptPreview running clip started preview = do
  speaking <- await running "live_check_preview_no_audio" 30000000 (\_ -> audibleSince started)
  continue (const interrupt) speaking
  where
    interrupt = do
      began <- getMonotonicTimeNSec
      end <- say running clip
      result <- within (failure "live_check_interruption_timeout") 12000000 (Right <$> waitCatch preview)
      continue (resumed began end) (result >>= interruptedPreview)
    resumed began end stopped = do
      response <- await running "live_check_resume_after_interruption_failed" 35000000 (answered end ["four", "4"])
      pure (report <$> response)
      where report trace = object ["interruption_ms" .= milliseconds began stopped, "resumed_answer" .= turnTiming end trace]

timePreview :: IO (Result Bool) -> IO (Word64, Result Bool)
timePreview action = do
  result <- action
  ended <- getMonotonicTimeNSec
  pure (ended, result)

interruptedPreview :: Either SomeException (Word64, Result Bool) -> Result Word64
interruptedPreview (Right (ended, Right False)) = Right ended
interruptedPreview (Right (_, Left issue)) = Left issue
interruptedPreview _ = Left (failure "live_check_preview_was_not_interrupted")

hangupCheck :: Settings -> RawSend -> IO (Result Value)
hangupCheck settings sender = spoken settings sender "Please hang up now." >>= continue prepare
  where
    prepare clip = continue (run clip) (endConfiguration settings)
    run clip rules = withConversation settings schemas (endExecute settings rules) (hangupAction clip)
    schemas = toolDefinitions settings "END_CALL_TOOLS"

endExecute :: Settings -> EndConfig -> Harness -> Execute
endExecute settings rules harness _ arguments ident = do
  current <- readTVarIO (harnessControls harness)
  let installed = maybe (Left (failure "live_check_controls_missing")) Right current
  continue run ((,) <$> installed <*> parseEndReason arguments)
  where
    run (current, reason) = executeEndCall rules (endContext current ident) (endEffects settings harness current) reason

endContext :: LiveControls -> Text -> IO EndContext
endContext current ident = EndContext True False False False <$> hasPendingTools current ident

endEffects :: Settings -> Harness -> LiveControls -> EndEffects
endEffects settings harness current = EndEffects
  { announceFarewell = announce settings current
  , terminateCall = finishCall (media harness)
  , markEnded = stopSession current
  }

announce :: Settings -> LiveControls -> Text -> Text -> IO (Result Bool)
announce settings current line completion = continue play configured
  where
    configured = (,) <$> announcementConfig settings line <*> rule settings "protocol" "ownerAnnouncementQuietMicros"
    play (cfg, quiet) = playAnnouncement current cfg completion quiet True

hangupAction :: ByteString -> Running -> IO (Result Value)
hangupAction clip running = do
  end <- say running clip
  finished <- within (failure "live_check_hangup_timeout") 40000000 (Right <$> waitCatch (runningTask running))
  trace <- snapshot running
  now <- getMonotonicTimeNSec
  pure (hangupResult end now trace finished)

hangupResult :: Word64 -> Word64 -> Trace -> Result (Either SomeException (Result ())) -> Result Value
hangupResult end now trace finished = do
  outcome <- finished
  closedResult False outcome
  ensure "live_check_hangup_not_requested" (traceFinished trace)
  ensure "live_check_hangup_tool_count" (calls trace == ["end_call"])
  ensure "live_check_farewell_not_audible" (audibleSince end trace)
  Right (object ["hangup_ms" .= milliseconds end now])

screeningCheck :: Settings -> RawSend -> IO (Result Value)
screeningCheck settings sender = spoken settings sender text >>= continue prepare
  where
    text = "Hello, my name is Alex. This is an unsolicited marketing call to sell solar panels. I do not know Sam personally."
    selected = settings {settingsValues = Map.insert "INCOMING_OWNER_NAME" "Sam" (settingsValues settings)}
    prepare clip = continue (run clip) (restrictedSettings selected False False)
    run clip cfg = do
      decision <- newTVarIO Nothing
      withConfigured cfg (const (screenExecute decision)) (screenAction decision clip)

screenExecute :: TVar (Maybe Decision) -> Execute
screenExecute state name arguments _ = do
  previous <- readTVarIO state
  continue save (evaluateDecision False False previous name arguments)
  where
    save decision = atomically (writeTVar state (Just decision)) >> pure (Right (resultValue "decision_received"))

screenAction :: TVar (Maybe Decision) -> ByteString -> Running -> IO (Result Value)
screenAction decision clip running = do
  greeting <- await running "live_check_screening_greeting_timeout" 30000000 (greeted since)
  continue (const speak) greeting
  where
    since = harnessStarted (runningHarness running)
    speak = do
      end <- say running clip
      submitted <- await running "live_check_screening_decision_timeout" 35000000 (\_ -> seen "check.tool.submitted")
      selected <- readTVarIO decision
      pure (screenResult running end selected submitted)

greeted :: Word64 -> Word64 -> Trace -> Bool
greeted since now trace = audibleSince since trace && quietAfter since now trace

screenResult :: Running -> Word64 -> Maybe Decision -> Result Trace -> Result Value
screenResult running end selected submitted = do
  trace <- submitted
  ensure "live_check_screening_exposed_owner_tools" (names == ["screen_call"])
  ensure "live_check_screening_wrong_tool" (calls trace == ["screen_call"])
  ensure "live_check_screening_wrong_decision" (declined selected)
  Right (object ["decision" .= ("decline_marketing" :: Text), "decision_ms" .= elapsed trace])
  where
    names = advertisedNames (liveSession (harnessConfig (runningHarness running)))
    elapsed trace = case reverse (eventTimes "check.tool.submitted" trace) of
      at : _ -> milliseconds end at
      [] -> 0
    declined (Just (Decline "marketing" _ _)) = True
    declined _ = False

cancellationCheck :: Settings -> IO (Result Value)
cancellationCheck settings = withConversation settings [] (const unavailable) cancelRunning
  where
    cancelRunning running = do
      active <- await running "live_check_audio_input_timeout" 5000000 (\_ -> seen "check.input.audio")
      continue (const (cancelConversation running)) active


greetingResult :: Settings -> IO CheckResult
greetingResult settings
  | Text.null (setting settings "PHONE_ASSISTANT_GREETING") = skip "live_configured_greeting" "greeting_disabled"
  | otherwise = check "live_configured_greeting" (greetingCheck settings)

greetingCheck :: Settings -> IO (Result Value)
greetingCheck settings = do
  now <- getCurrentTime
  let configured = buildSession settings now [] >>= (\session -> liveConfiguration settings session greeting)
  continue (\cfg -> withConfigured cfg (const unavailable) inspect) configured
  where
    greeting = setting settings "PHONE_ASSISTANT_GREETING"
    inspect running = do
      let since = harnessStarted (runningHarness running)
      result <- await running "live_check_greeting_timeout" 35000000 (greetingHeard greeting since)
      pure (object ["configured_greeting_spoken" .= True] <$ result)

greetingHeard :: Text -> Word64 -> Word64 -> Trace -> Bool
greetingHeard expected since now trace = announcementComplete True ending (answerSince since trace)
  && audibleSince since trace && quietAfter since now trace
  where ending = Text.unwords (takeLast 5 (Text.words expected))

webResult :: Settings -> RawSend -> IO CheckResult
webResult settings sender = case booleanSetting settings "PHONE_ASSISTANT_WEB_SEARCH" of
  Right True -> check "live_hosted_web_search" (webCheck settings sender)
  Right False -> skip "live_hosted_web_search" "web_search_disabled"
  Left issue -> check "live_hosted_web_search" (pure (Left issue))

webCheck :: Settings -> RawSend -> IO (Result Value)
webCheck settings sender = spoken settings sender question >>= continue prepare
  where
    question = "Please search the web using the official Science Museum London website. Tell me the street and district it is on, in one sentence."
    schemas = toolDefinitions settings "WEB_SEARCH_TOOLS"
    prepare clip = do
      now <- getCurrentTime
      let configured = buildSession settings now schemas >>= (\session -> liveConfiguration settings session "")
      continue (\cfg -> withConfigured cfg (const unavailable) (webAction clip)) configured

webAction :: ByteString -> Running -> IO (Result Value)
webAction clip running = do
  end <- say running clip
  result <- await running "live_check_web_search_timeout" 75000000 (webAnswered end)
  pure (webReport end <$> result)

webAnswered :: Word64 -> Word64 -> Trace -> Bool
webAnswered since now trace = not (null completed) && seen "response.completed" trace
  && hasWords ["exhibition"] content && hasWords ["kensington"] content
  && audibleSince since trace && quietAfter since now trace
  where
    completed = webSearchTimes trace
    content = answerSince (foldr max since completed) trace

webReport :: Word64 -> Trace -> Value
webReport end trace = object ["completed_web_search_calls" .= length (webSearchTimes trace),
  "official_location_answered" .= True, "answer" .= turnTiming end trace]
