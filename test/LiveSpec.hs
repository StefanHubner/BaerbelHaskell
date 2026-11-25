{-# LANGUAGE OverloadedStrings #-}
module LiveSpec (spec) where

import Baerbel.Core.Concurrent (hasElapsed)
import Baerbel.Core.Background (BackgroundRules(..), Progress(..), progressStep)
import qualified Data.Set as Set
import qualified Baerbel.Runtime.Config as Config
import qualified Baerbel.Runtime.Ringback as Ring
import qualified Data.Map.Strict as Map
import qualified Baerbel.Runtime.Pin as Pin
import qualified Baerbel.Runtime.EndCall as End
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import System.Timeout (timeout)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Baerbel.Core.Error (Failure(..), failure)
import Baerbel.Core.JSON (field)
import Baerbel.Runtime.Live
import Baerbel.Core.Protocol
import Baerbel.Runtime.Speech
import Data.Aeson (Value(..), object, (.=), toJSON, eitherDecodeFileStrict')
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Test.Hspec
import qualified LiveCloseSpec
import qualified PacingSpec
import qualified PinVerificationSpec

limits :: ToolLimits
limits = ToolLimits 8 16 128 16000

event :: Text -> Value -> Value
event delegation inner = object ["delegation_id" .= delegation, "event" .= inner]

created :: Value
created = event "d1" (object ["type" .= ("response.created" :: Text), "response" .= object ["id" .= ("r1" :: Text)]])

callEvent :: Text -> Text -> Value
callEvent name args = event "d1" (object ["type" .= ("response.output_item.done" :: Text),
  "response_id" .= ("r1" :: Text), "item" .= object ["type" .= ("function_call" :: Text),
  "call_id" .= ("c1" :: Text), "name" .= name, "arguments" .= args]])

completed :: Text -> Value
completed kind = event "d1" (object ["type" .= kind, "response" .= object ["id" .= ("r1" :: Text)]])

advance :: [Value] -> Either Failure (ToolState, [ToolBatch])
advance = foldl step (Right (emptyToolState, []))
  where
    step result item = result >>= \(state, work) -> do
      (next, added) <- stepTools limits item state
      pure (next, work ++ added)

spec :: Spec
spec = do
  LiveCloseSpec.spec
  PacingSpec.spec
  describe "Live setup classification" $ do
    it "distinguishes startup, provider failure, closure and unrelated events" $ do
      let classify kind = observeStarted (object ["type" .= (kind :: Text)])
      map classify ["session.started", "error", "session.closed", "session.input_transcript.delta"]
        `shouldBe` [Right True, Left (failure "live_setup_rejected"), Left (failure "live_closed_before_start"), Right False]
  describe "Announcement transcript updates" $ do
    it "does not request a state write for unrelated or malformed events" $ do
      let unrelated = object ["type" .= ("session.started" :: Text), "delta" .= ("ignored" :: Text)]
          malformed = object ["type" .= ("session.output_transcript.delta" :: Text), "delta" .= (1 :: Int)]
      map (fmap ($ "existing") . appendTranscript) [unrelated, malformed] `shouldBe` [Nothing, Nothing]
    it "appends valid output while retaining the existing transcript bound" $ do
      let eventValue = object ["type" .= ("session.output_transcript.delta" :: Text), "delta" .= ("new" :: Text)]
          previous = T.replicate 2000 "x"
          expected = T.replicate 1997 "x" <> "new"
      fmap ($ previous) (appendTranscript eventValue) `shouldBe` Just expected
  describe "Live tool transactions" $ do
    it "does not execute a tool until its response completes" $ do
      let result = advance [created, callEvent "gmail_send" "{}"]
      fmap (map toolName . concatMap batchCalls . snd) result `shouldBe` Right []
    it "submits exactly the completed response batch" $ do
      let result = advance [created, callEvent "gmail_send" "{}", completed "response.completed"]
      fmap (map toolName . concatMap batchCalls . snd) result `shouldBe` Right ["gmail_send"]
    it "rejects reuse of a call ID with different arguments" $ do
      let result = advance [created, callEvent "gmail_send" "{}", callEvent "gmail_send" "{\"changed\":true}"]
      result `shouldBe` Left (failure "live_tool_id_reused")
    it "ignores an exact replay after a completed batch" $ do
      let result = advance [created, callEvent "gmail_send" "{}", completed "response.completed", completed "response.completed"]
      fmap (length . snd) result `shouldBe` Right 1
    it "never executes partial actions after response failure" $ do
      let result = advance [created, callEvent "gmail_send" "{}", completed "response.failed"]
      fmap (length . snd) result `shouldBe` Right 0
      fmap (cachedOutput "c1" . fst) result `shouldSatisfy` either (const False) (/= Nothing)
    it "rejects function calls without an open response" $ do
      stepTools limits (callEvent "gmail_send" "{}") emptyToolState `shouldSatisfy` isLeft
    it "retains completed receipts for executor deduplication" $ do
      let receipt = object ["status" .= ("ok" :: Text)]
          state = rememberOutput "c1" receipt emptyToolState
      cachedOutput "c1" state `shouldBe` Just receipt
  describe "PCM streaming" $ do
    it "keeps the remainder when arbitrary packets split a frame" $ do
      let bytes = BS.replicate 327 1
          (frames, rest) = splitFrames 320 bytes
      map BS.length frames `shouldBe` [320]
      BS.length rest `shouldBe` 7
    it "round trips signed little-endian PCM16 boundaries" $ do
      let samples = [-32768, -1, 0, 1, 32767]
      decodePCM (encodePCM samples) `shouldBe` Right samples
    it "rejects incomplete PCM samples" $ do
      decodePCM (BS.singleton 0) `shouldSatisfy` isLeft
    it "rejects invalid base64 output audio" $ do
      decodeAudio (object ["delta" .= ("not base64!" :: Text)]) `shouldSatisfy` isLeft
    it "distinguishes low noise from audible output" $ do
      audiblePCM (encodePCM [0, 20, -20]) `shouldBe` False
      audiblePCM (encodePCM [0, 1000, -1000]) `shouldBe` True
    it "resamples 8 kHz PCM to 24 kHz through native HQ soxr" $ do
      let pcm = encodePCM (replicate 800 1000)
      result <- withResampler 8000 24000 $ \converter -> do
        first <- resample converter pcm False
        tailPCM <- resample converter BS.empty True
        pure ((<>) <$> first <*> tailPCM)
      fmap BS.length result `shouldBe` Right 4800
  describe "session permission boundary" $ do
    let owner = object ["name" .= ("gmail_search" :: Text)]
        screening = object ["name" .= ("screen_call" :: Text)]
        message = object ["name" .= ("take_message" :: Text)]
        tools = [owner, screening, message, object ["type" .= ("web_search" :: Text)]]
        session = SessionSettings Owner "gpt-live-1" "willow" "voice" "gpt-6.2-sol" "backend" "low" "priority" 100 tools 24000
        responses role = field "delegation" (sessionConfig session {sessionRole = role}) >>= field "responses"
    it "restricts public sessions to screening and messages" $ do
      allowedTools Restricted tools `shouldBe` [screening, message]
    it "gives announcements no tools" $ do
      allowedTools Announcement tools `shouldBe` []
    it "preserves owner tool ordering" $ do
      allowedTools Owner tools `shouldBe` tools
    it "does not inject an empty greeting" $ do
      greetingCommand "Say {greeting}" "" `shouldBe` Nothing
    it "configures Sol with low reasoning and priority for managed delegation" $ do
      let backend = responses Owner
      (backend >>= field "model") `shouldBe` Just (String "gpt-6.2-sol")
      (backend >>= field "reasoning" >>= field "effort") `shouldBe` Just (String "low")
      (backend >>= field "service_tier") `shouldBe` Just (String "priority")
    it "retains the restricted tool boundary when priority is selected" $ do
      let backend = responses Restricted
      (backend >>= field "service_tier") `shouldBe` Just (String "priority")
      (backend >>= field "tools") `shouldBe` Just (toJSON [screening, message])
    it "keeps announcements on client delegation without a backend tier" $ do
      let configured = sessionConfig session {sessionRole = Announcement}
          client = object ["type" .= ("client" :: Text)]
      field "delegation" configured `shouldBe` Just client
  describe "local PIN state" $ do
    it "clears digits with star and never submits an overlong PIN" $ do
      let entered = Pin.PinState "12345678" False False
          (overflow, _) = Pin.keypadStep 8 "9" entered
          (_, submitted) = Pin.keypadStep 8 "#" overflow
      submitted `shouldBe` Just BS.empty
      fst (Pin.keypadStep 8 "*" entered) `shouldBe` Pin.emptyPinState
    it "ignores spoken-looking data and non-keypad control bytes" $ do
      Pin.keypadStep 8 "verified" Pin.emptyPinState `shouldBe` (Pin.emptyPinState, Nothing)
    it "requires exactly the original PBKDF2 hash format" $ do
      isLeft (Pin.parsePinHash "pbkdf2_sha256:1:0000:0000") `shouldBe` True
      let uppercase = "pbkdf2_sha256:600000:" <> T.replicate 32 "A" <> ":" <> T.replicate 64 "0"
      isLeft (Pin.parsePinHash uppercase) `shouldBe` True
    it "keeps PIN retry notification off the audio reader and cancels it on close" $ do
      let hash = "pbkdf2_sha256:600000:" <> T.replicate 32 "0" <> ":" <> T.replicate 64 "0"
          config = Pin.PinConfig hash ["gmail_"] 60000000 4 8 (String "prompt") (String "incorrect") (String "verified")
      opened <- Pin.newPinGate config
      case opened of
        Left issue -> expectationFailure (show issue)
        Right gate -> do
          result <- timeout 1000000 (finally (exercisePin gate) (Pin.closePin gate))
          result `shouldBe` Just (Left (failure "pin_unavailable"))
  PinVerificationSpec.spec
  describe "owner call ending" $ do
    let ready = End.EndContext True False False False False
        reason value = object ["reason" .= (value :: Text)]
    it "requires an explicit supported reason" $ do
      End.parseEndReason (object []) `shouldSatisfy` isLeft
      (End.parseEndReason (reason "owner_requested") >>= End.checkEndRequest ready) `shouldBe` Right End.OwnerRequested
    it "blocks inferred endings while notes or other tools are pending" $ do
      let busy = ready {End.endNotesPending = True}
      End.checkEndRequest busy End.ConversationComplete `shouldSatisfy` isLeft
      End.checkEndRequest busy End.OwnerRequested `shouldBe` Right End.OwnerRequested
    it "does not end a public or natively bridged call" $ do
      End.checkEndRequest (ready {End.endOwner = False}) End.OwnerRequested `shouldSatisfy` isLeft
      End.checkEndRequest (ready {End.endPaused = True}) End.OwnerRequested `shouldSatisfy` isLeft
    it "preserves the call if the owner interrupts the farewell" $ do
      ended <- newIORef False
      let effects = End.EndEffects (\_ _ -> pure (Right False)) (writeIORef ended True >> pure (Right ())) (pure ())
          config = End.EndConfig "Goodbye" "Goodbye" 100000
      _ <- End.executeEndCall config (pure ready) effects End.OwnerRequested
      readIORef ended `shouldReturn` False
    it "still hangs up when the bounded farewell fails" $ do
      ended <- newIORef False
      let effects = End.EndEffects (\_ _ -> pure (Left (failure "provider_failed"))) (writeIORef ended True >> pure (Right ())) (pure ())
          config = End.EndConfig "Goodbye" "Goodbye" 100000
      _ <- End.executeEndCall config (pure ready) effects End.OwnerRequested
      readIORef ended `shouldReturn` True
  describe "announcement completion" $ do
    it "waits for delivered speech to finish and the exact quiet interval to elapse" $ do
      let ready now = announcementReady False "Goodbye" 600000 now "Goodbye!" (Just 1000000000)
      map ready [999999999, 1000000000, 1599999999, 1600000000]
        `shouldBe` [False, False, False, True]
      announcementReady False "Goodbye" 600000 1600000000 "Goodbye!" Nothing `shouldBe` False
    it "retains different transcript rules for interruptible and one-shot playback" $ do
      let ready marker = announcementReady marker "Goodbye" 600000 1600000000 "Goodbye. Extra words" (Just 1000000000)
      map ready [False, True] `shouldBe` [False, True]
    it "recognizes contractions and normalized accented names" $ do
      announcementComplete True "I will connect you now" "I'll connect you now." `shouldBe` True
      spokenWords "Bärbel, I'm ready" `shouldBe` ["barbel", "i", "am", "ready"]
    it "normalizes only complete GPT-Live model names" $ do
      spokenWords "G.P.T. Live One" `shouldBe` ["gpt", "live", "1"]
      spokenWords "Live One" `shouldBe` ["live", "one"]
    it "requires the complete ending for one-shot announcements" $ do
      announcementComplete False "What can I do for you" "What can I do for you?" `shouldBe` True
      announcementComplete False "What can I do for you" "What can I do for you? Extra words" `shouldBe` False
      announcementComplete True "Bye bye" "Okay. Bye bye!" `shouldBe` True
  describe "startup ringback" $ do
    it "uses the original UK cadence without mixing silence into speech" $ do
      let rules = Ring.RingRules [400, 450] 1200 160 8000 20000 150 [(0,20),(30,50)]
      map (audiblePCM . Ring.ringFrame rules) [0,19,20,29,30,49,50,149,150] `shouldBe`
        [True,True,False,False,True,True,False,False,True]
      BS.length (Ring.ringFrame rules 0) `shouldBe` 320

  describe "provider handoff recovery" $ do
    it "never executes partial tools after an incomplete handoff" $ do
      let recovered = fmap (abandonUncommitted (maxToolCalls limits) . fst) (advance [created, callEvent "gmail_send" "{}"])
      fmap (cachedOutput "c1") recovered `shouldSatisfy` either (const False) (/= Nothing)
    it "preserves already committed work and its existing deduplication state" $ do
      let recovered = fmap (abandonUncommitted (maxToolCalls limits) . fst) (advance [created, callEvent "gmail_send" "{}", completed "response.completed"])
      fmap (cachedOutput "c1") recovered `shouldBe` Right Nothing
    it "recovers only the exact known provider error and never recursive notices" $ do
      let message ident = object ["error" .= object ["type" .= ("invalid_request_error" :: Text),
            "code" .= ("invalid_request_error" :: Text), "message" .= ("Responses handoff incomplete." :: Text),
            "param" .= Null, "client_event_id" .= (ident :: Text)]]
      recoverable Set.empty (message "server") `shouldBe` True
      recoverable (Set.singleton "notice") (message "notice") `shouldBe` False
      recoverable Set.empty (object ["error" .= object ["message" .= ("other" :: Text)]]) `shouldBe` False
  describe "concurrent quiet-time observation" $ do
    it "does not underflow when playback advances after the clock snapshot" $ do
      hasElapsed 600000 1000000000 1000000001 `shouldBe` False
      hasElapsed 600000 1600000000 1000000000 `shouldBe` True
  describe "background progress" $ do
    let rules = BackgroundRules 500000 6000000 20000000 "active" "idle" "working" "recover" "retry"
    it "announces state changes before offering a quiet-time update" $ do
      let (progress, changes) = progressStep rules 1000000000 1000000000 True False (Progress False 0)
          (_, beforeQuiet) = progressStep rules 2000000000 1000000000 True False progress
          (_, ready) = progressStep rules 7000000000 1000000000 True False progress
      length changes `shouldBe` 1
      beforeQuiet `shouldBe` []
      length ready `shouldBe` 1
    it "suppresses spoken updates during private input, notes and native actions" $ do
      let (_, messages) = progressStep rules 10000000000 0 True True (Progress True 0)
      messages `shouldBe` []

  describe "configured session payloads" $ do
    it "matches the complete default session at a fixed clock" $ do
      global <- eitherDecodeFileStrict' "config/global.json" :: IO (Either String Config.Global)
      original <- eitherDecodeFileStrict' "test/fixtures/live-session.json" :: IO (Either String Value)
      case (global, original) of
        (Right values, Right expected) -> do
          let settings = Config.resolveSettings values (Config.User Map.empty)
              tools = Config.toolDefinitions settings "WEB_SEARCH_TOOLS" ++ Config.toolDefinitions settings "HELP_TOOLS"
              day = fromGregorian 2026 11 19
              time = secondsToDiffTime 43200
              now = UTCTime day time
          fmap sessionConfig (buildSession settings now tools) `shouldBe` Right expected
        _ -> expectationFailure "Could not load preserved session fixture or global configuration"

    it "matches every enabled owner policy and tool at the UK DST transition" $ do
      global <- eitherDecodeFileStrict' "config/global.json" :: IO (Either String Config.Global)
      original <- eitherDecodeFileStrict' "test/fixtures/live-integrated-session.json" :: IO (Either String Value)
      case (global, original) of
        (Right values, Right expected) -> integratedSession values `shouldBe` Right expected
        _ -> expectationFailure "Could not load integrated session fixture or global configuration"

integratedSession :: Config.Global -> Either Failure Value
integratedSession global = do
  forecast <- Config.rule settings "windguru" "WINDGURU_SET_TOOL"
  let first = concatMap (Config.toolDefinitions settings) ["WEB_SEARCH_TOOLS", "PROFILE_TOOLS", "WINDGURU_TOOLS"]
      rest = concatMap (Config.toolDefinitions settings) ["GMAIL_TOOLS", "OWNER_NOTE_TOOLS", "CALENDAR_TOOLS",
        "MAPS_TOOLS", "CONTACTS_TOOLS", "DIRECT_CALL_TOOLS", "VOICE_TEST_TOOLS", "END_CALL_TOOLS", "HELP_TOOLS"]
      tools = first ++ [forecast] ++ rest
      day = fromGregorian 2026 3 29
      time = secondsToDiffTime 43200
      now = UTCTime day time
  sessionConfig <$> buildSessionWithProfile settings now "PROFILE CONTEXT" tools
  where
    pin = "pbkdf2_sha256:600000:" <> T.replicate 32 "0" <> ":" <> T.replicate 64 "0"
    user = Config.User (Map.singleton "PHONE_ASSISTANT_PRIVATE_PIN_HASH" pin)
    settings = Config.resolveSettings global user

exercisePin :: Pin.PinGate -> IO (Either Failure ())
exercisePin gate = do
  prompted <- newEmptyMVar
  retrying <- newEmptyMVar
  blocked <- newEmptyMVar
  let send command = notify command >> pure (Right ())
      notify (String "prompt") = putMVar prompted ()
      notify _ = putMVar retrying () >> takeMVar blocked
  withAsync (Pin.prepareTool gate send "gmail_search") $ \worker -> do
    takeMVar prompted
    Pin.feedDigit gate "#"
    takeMVar retrying
    Pin.closePin gate
    wait worker
