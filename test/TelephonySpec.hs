{-# LANGUAGE OverloadedStrings #-}
module TelephonySpec (spec) where

import qualified Baerbel.Runtime.AMI as A
import Baerbel.Runtime.AudioSocket
import Baerbel.Core.CallState
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.Incoming
import Baerbel.Core.IncomingSession (restrictedSettings)
import Baerbel.Core.Config (Global, User(..), Settings(..), resolveSettings)
import Baerbel.Runtime.Live (LiveConfig(..), Media(receiveAudio))
import Baerbel.Runtime.ManagedMedia (ManagedCall(..), newRegistry, withManaged)
import Baerbel.Core.PhoneNumber (PhoneMetadata, validateNumber)
import TestSupport (withRule)
import Baerbel.Runtime.Recording
import qualified Baerbel.Runtime.Telephony as Native
import Baerbel.Core.Routing
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever, unless, void)
import System.Timeout (timeout)
import Control.Exception (bracket)
import Data.Aeson (Value(..), encode, FromJSON(..), eitherDecodeStrict', withObject, (.:), object, (.=))
import qualified Data.Aeson.Key
import qualified Data.Vector
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Socket (Socket, Family(AF_UNIX, AF_INET), SocketType(Stream), SockAddr(SockAddrInet), SocketOption(ReuseAddr), accept, bind, close, defaultProtocol, listen, setSocketOption, socket, socketPair, tupleToHostAddress, shutdown, ShutdownCmd(ShutdownBoth))
import qualified Network.Socket.ByteString as N
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (setFileTimesHiRes)
import Test.Hspec
import Test.QuickCheck hiding (Failure, Result, collect)

spec :: Spec
spec = do
  describe "AudioSocket" audioSpec
  describe "AMI isolation and parsing" amiSpec
  describe "Outbound routing" routingSpec
  describe "Restricted incoming calls" incomingSpec
  describe "Public session composition" publicSpec
  describe "Native call state" callSpec
  describe "Call recording" recordingSpec
  describe "Pure number metadata" phoneSpec
  describe "Native call interpreter" nativeSpec
  describe "Managed media boundaries" managedSpec

audioSpec :: Spec
audioSpec = do
  it "round trips arbitrary PCM payloads without changing byte order" $ property $ forAll pcm $ \bytes ->
    let packet = Packet Audio bytes in (encodePacket packet >>= decodePacket) == Right (packet, B.empty)
  it "preserves the following packet after decoding one frame" $ do
    let packet = Packet Dtmf "#"
    let encoded = encodePacket packet
    (encoded >>= decodePacket . (<> "tail")) `shouldBe` Right (packet, "tail")
  it "rejects incomplete, odd PCM, malformed UUID and nonempty hangup" $ do
    encodePacket (Packet Audio "x") `shouldBe` Left (failure "audiosocket_invalid_audio")
    encodePacket (Packet CallId "short") `shouldBe` Left (failure "audiosocket_invalid_uuid")
    encodePacket (Packet Hangup "x") `shouldBe` Left (failure "audiosocket_invalid_hangup")
    decodePacket (B.pack [16, 0, 4, 97, 98]) `shouldBe` Left (failure "audiosocket_incomplete_payload")
  it "formats UUID identity canonically" $ do
    uuidText (B.pack [0..15]) `shouldBe` Right "00010203-0405-0607-0809-0a0b0c0d0e0f"
  it "receives a frame fragmented across TCP writes" $ bracket sockets closeSockets $ \(reader, writer) -> do
    let fragments = map B.singleton [16, 0, 4, 1, 2, 3, 4]
    withAsync (mapM_ (N.sendAll writer) fragments) $ \_ ->
      readPacket reader `shouldReturn` Right (Packet Audio (B.pack [1,2,3,4]))
  where
    pcm :: Gen B.ByteString
    pcm = do
      size <- chooseInt (1, 1000)
      bytes <- vectorOf (size * 2) arbitrary
      pure (B.pack bytes)
    sockets :: IO (Socket, Socket)
    sockets = socketPair AF_UNIX Stream defaultProtocol
    closeSockets (left, right) = close left >> close right

amiSpec :: Spec
amiSpec = do
  it "uses configured action-field and event-count limits" $ do
    let limited = amiTestRules {A.amiFieldChars = 8, A.amiEventLimit = 1}
        event = A.parseMessage "Event: Status\r\n\r\n"
    A.encodeAction limited "Status" "one" [("Channel", "123456789")] `shouldBe` Left (failure "ami_invalid_action")
    A.actionResponse limited (Just "StatusComplete") [event] event `shouldBe` Left (failure "ami_event_limit")

  it "preserves multiple Variable headers and splits values at the first equals" $ do
    let message = A.parseMessage "Event: Status\r\nVariable: PA_CALL_ID=one\r\nVariable: A=b=c\r\n\r\n"
    A.variable "PA_CALL_ID" message `shouldBe` "one"
    A.variable "A" message `shouldBe` "b=c"
  it "rejects action field injection before any network write" $ do
    A.encodeAction amiTestRules "Redirect" "one" [("Channel", "name\r\nAction: Originate")] `shouldBe` Left (failure "ami_invalid_action")
  it "requires exactly one PBX-authorized owner channel" $ do
    let owner = status "owner" "1" "phone-assistant-inbound" ""
    A.ownerChannel amiTestRules "call" [owner] `shouldBe` Right channel
    A.ownerChannel amiTestRules "call" [owner, owner] `shouldBe` Left (failure "ami_call_not_found")
    A.ownerChannel amiTestRules "call" [status "screening" "0" "phone-assistant-screening" "screening"] `shouldBe` Left (failure "ami_call_not_found")
  it "never promotes a public caller into the owner role" $ do
    let public = status "screening" "0" "phone-assistant-screening" "screening"
    A.incomingChannel "call" [public] `shouldBe` Right channel
    A.incomingChannel "different" [public] `shouldBe` Left (failure "incoming_call_not_found")
    A.incomingChannel "call" [status "owner" "1" "phone-assistant-inbound" ""] `shouldBe` Left (failure "incoming_call_not_found")
  it "completes status collection in received order" $ do
    let first = A.parseMessage "Event: Status\r\nChannel: one\r\n"
    let second = A.parseMessage "Event: Status\r\nChannel: two\r\n"
    let completed = A.parseMessage "Event: StatusComplete\r\n"
    A.actionResponse amiTestRules (Just "StatusComplete") [second, first] completed `shouldBe` Right (Just [first, second])

status :: Text -> Text -> Text -> Text -> A.Message
status permission authorized nativeContext mode = A.Message fields variables
  where
    fields :: M.Map Text Text
    fields = M.fromList [("Event", "Status"), ("Context", nativeContext), ("Application", "AudioSocket"), ("Channel", "PJSIP/phone-assistant-aa-000000ab"), ("Uniqueid", "123.4")]
    variables :: M.Map Text Text
    variables = M.fromList [("PA_CALL_ID", "call"), ("PA_PERMISSION", permission), ("PA_OWNER_AUTHORIZED", authorized), ("PA_INCOMING_MODE", mode)]

channel :: A.Channel
channel = A.Channel "PJSIP/phone-assistant-aa-000000ab" "123.4" False

routingSpec :: Spec
routingSpec = do
  it "normalizes explicit international separators without choosing a country" $ do
    normalizeDestination "0044 (20) 7946-0018" `shouldBe` Right "+442079460018"
    normalizeDestination "02079460018" `shouldBe` Left (failure "international_number_required")
    normalizeDestination "+44;123" `shouldBe` Left (failure "international_number_required")
  it "uses the injected number classification and preserves its rejection" $ do
    selectRoute (const (Left (failure "destination_class_not_permitted"))) settings Owner "+449001234567" `shouldBe` Left (failure "destination_class_not_permitted")
  it "routes Austrian calls only through enabled available Telematica" $ do
    selectRoute Right settings Owner "+4312345678" `shouldBe` Right (Route "+4312345678" Telematica False)
    selectRoute Right (settings {telematicaEnabled = False}) Owner "+4312345678" `shouldBe` Left (failure "telematica_unavailable_no_automatic_fallback")
  it "requires owner permission and extra confirmation outside UK/Austria" $ do
    selectRoute Right settings Screening "+442079460018" `shouldBe` Left (failure "owner_required")
    selectRoute Right settings Owner "+33123456789" `shouldBe` Right (Route "+33123456789" AA True)
  it "speaks only the destination suffix in the ordinary announcement" $ do
    announcement (Route "+442079460018" AA False) (Just "Alice") `shouldBe` "Calling Alice, ending in 0 1 8 via A and A. I'll connect you now."
  where
    settings :: RouteSettings
    settings = RouteSettings True True True

incomingSpec :: Spec
incomingSpec = do
  it "accepts an uncertain legitimate forwarding decision" $ do
    evaluateDecision False False Nothing "screen_call" screen `shouldBe` Right (Forward "uncertain" "" "")
  it "forbids forwarding from message-only sessions" $ do
    evaluateDecision True True Nothing "screen_call" screen `shouldBe` Left (failure "invalid_screening_decision")
  it "rejects extra recipient fields and unavailable message submission" $ do
    evaluateDecision True False Nothing "take_message" injected `shouldBe` Left (failure "invalid_message")
    evaluateDecision False False Nothing "take_message" message `shouldBe` Left (failure "invalid_message")
  it "accepts plain multiline message text and enforces one decision" $ do
    let decision = LeaveMessage "Alice" "Please call\nwhen free"
    evaluateDecision True False Nothing "take_message" message `shouldBe` Right decision
    evaluateDecision True False (Just decision) "screen_call" screen `shouldBe` Left (failure "screening_already_decided")
  it "uses trusted privacy metadata before exposing a callback" $ do
    callbackNumber "allowed" "+442079460018" `shouldBe` Just "+442079460018"
    callbackNumber "withheld" "+442079460018" `shouldBe` Nothing
    remainingBudget "61" `shouldBe` Left (failure "incoming_invalid_time_budget")
  where
    screen :: Value
    screen = object ["decision" .= ("forward" :: Text), "reason" .= ("uncertain" :: Text)]
    message :: Value
    message = object ["caller_name" .= ("Alice" :: Text), "message" .= ("Please call\nwhen free" :: Text)]
    injected :: Value
    injected = object ["caller_name" .= ("Alice" :: Text), "message" .= ("Hello" :: Text), "recipient" .= ("attacker" :: Text)]

callSpec :: Spec
callSpec = do
  it "does not hang up a possibly accepted incoming redirect" $ do
    let state = (initialCall "call" Screening channel) {callPhase = RedirectPending}
    let uncertain = forwardResult (Left (Failure "ami_action_outcome_unknown" True)) state
    requestTerminate True uncertain `shouldBe` (uncertain, [])
  it "allows cleanup after a definitively rejected redirect" $ do
    let state = (initialCall "call" Screening channel) {callPhase = RedirectPending}
    let rejected = forwardResult (Left (failure "ami_action_rejected")) state
    callPhase rejected `shouldBe` Conversing
    snd (requestTerminate True rejected) `shouldBe` [HangupChannel (A.channelName channel)]
  it "requires completion of outstanding work for implicit call completion" $ do
    let state = initialCall "call" Owner channel
    finishCall False True False state `shouldBe` Left (failure "finish_pending_work_before_ending_call")
    finishCall True True False state `shouldBe` Right (state {callPhase = Ended}, [HangupChannel (A.channelName channel)])
  it "leaves the conversation active when the owner interrupts a farewell" $ do
    let state = initialCall "call" Owner channel
    finishCall True False True state `shouldBe` Right (state, [])

recordingSpec :: Spec
recordingSpec = do
  it "bounds an archive interval from adjacent flush positions" $ do
    let anchors = S.fromList [Anchor 10 0, Anchor 11 8000, Anchor 12 16000]
    sampleBounds anchors 10.5 `shouldBe` Right (4000, 4000)
    sampleBounds anchors 9 `shouldBe` Right (0, 0)
    sampleBounds anchors 13 `shouldBe` Right (16000, 16000)
  it "maps partial provider sends back to telephone timestamps" $ do
    let timeline = addFrame 20 (addFrame 10 emptyTimeline)
    let result = sentSamples 720 timeline
    fmap (transcriptBounds 0 30) result `shouldBe` Right (Just (10, 20.01))
    fmap (transcriptBounds 0 30 . resetTimeline) result `shouldBe` Right (Just (10, 20.01))
  it "rejects invalid recording duration before accessing the archive" $ do
    let state = ArchiveState S.empty 0 False
    clipBounds state 10 10 `shouldBe` Left (failure "recording_invalid_interval")
    clipBounds state 0 601 `shouldBe` Left (failure "recording_invalid_interval")
  it "opens a finalized PCM archive and reads its exact bounded bytes" $ bracket makeArchive removeArchive $ \(directory, identity, _) -> do
    opened <- openRecording directory identity
    case opened of
      Left problem -> expectationFailure (show problem)
      Right archive -> bracket (pure archive) closeRecording checkArchive

makeArchive :: IO (FilePath, Text, FilePath)
makeArchive = do
  directory <- getTemporaryDirectory
  (path, handle) <- openBinaryTempFile directory "baerbel-recording-test"
  hClose handle
  removeFile path
  let identity = T.pack (reverse (takeWhile (/= '/') (reverse path)))
  let wavePath = path ++ ".wav"
  let pcm = B.replicate 16000 1
  let format = B.pack [16,0,0,0,1,0,1,0,64,31,0,0,128,62,0,0,2,0,16,0]
  let header = "RIFF" <> B.pack [164,62,0,0] <> "WAVEfmt " <> format <> "data" <> B.pack [128,62,0,0]
  B.writeFile wavePath (header <> pcm)
  setFileTimesHiRes wavePath 1000 1000
  pure (directory, identity, wavePath)

removeArchive :: (FilePath, Text, FilePath) -> IO ()
removeArchive (_, _, path) = removeFile path

checkArchive :: Archive -> IO ()
checkArchive archive = do
  result <- readClip archive 999.25 999.75
  let state = ArchiveState (S.fromList [Anchor 999 0, Anchor 1000 8000]) 8000 True
  let expected = fmap (\(_, size) -> B.replicate size 1) (clipBounds state 999.25 999.75)
  result `shouldBe` expected
  readClip archive 1 2 `shouldReturn` Left (failure "recording_excerpt_exceeds_bounds")

data PhoneCase = PhoneCase Text Text Bool
newtype PhoneFixtures = PhoneFixtures [PhoneCase]

instance FromJSON PhoneCase where
  parseJSON = withObject "PhoneCase" $ \fields -> PhoneCase <$> fields .: "number" <*> fields .: "result" <*> fields .: "accepted"

instance FromJSON PhoneFixtures where
  parseJSON = withObject "PhoneFixtures" $ \fields -> PhoneFixtures <$> fields .: "cases"

phoneSpec :: Spec
phoneSpec = it "matches all 864 original Python fixtures with pure metadata validation" $ withRule "phone_numbers" "metadata" $ \metadata -> do
  bytes <- B.readFile "test/fixtures/phone_numbers.json"
  case eitherDecodeStrict' bytes of
    Left problem -> expectationFailure problem
    Right (PhoneFixtures cases) -> mapM_ (checkPhone metadata) cases

checkPhone :: PhoneMetadata -> PhoneCase -> IO ()
checkPhone metadata (PhoneCase number expected accepted) = do
  let actual = validateNumber metadata number
      result | accepted = Right expected
             | otherwise = Left (failure expected)
  actual `shouldBe` result

nativeSpec :: Spec
nativeSpec = do
  it "announces, dials once, resumes after BUSY, and caches the exact result" $ withFakeAMI $ \requests -> do
    opened <- Native.openOwner nativeConfig Right nativeMedia "call"
    case opened of
      Left problem -> expectationFailure (show problem)
      Right control -> bracket (pure control) Native.closeOwner (exerciseNative requests)
  it "does not redirect when the owner interrupts the route announcement" $ withFakeAMI $ \requests -> do
    let media = nativeMedia {Native.announceRoute = \_ _ -> pure (Right False)}
    checkNoDial requests media "owner_interrupted_before_dial"
  it "uses the configured AMI action deadline without a retry" $ withFakeAMIUsing unansweredStatus $ \requests -> do
    let cfg = nativeConfig {Native.amiRules = amiTestRules {A.amiActionMicros = 20000}}
    opened <- Native.openOwner cfg Right nativeMedia "call"
    void opened `shouldBe` Left (Failure "ami_action_outcome_unknown" True)
    actions <- readMVar requests
    length [request | request <- actions, A.field "Action" request == "Status"] `shouldBe` 1
  it "rechecks PBX owner authorization after the announcement before any dial" $ withFakeAMIUsing revokedFake $ \requests ->
    checkNoDial requests nativeMedia ""
  it "recovers an AMI disconnect without repeating the native dial" $ withFakeAMIUsing disconnectedFake $ \requests ->
    withOwner nativeMedia (checkDisconnected requests)
  it "does not revive an ended call when media recovery fails" $ withFakeAMI $ \requests -> do
    let media = nativeMedia {Native.waitResumed = \_ -> pure (Left (failure "call_resume_timeout"))}
    withOwner media (checkRecoveryFailure requests)
  it "keeps an answered native bridge alive until the matching Hangup" $ do
    active <- newEmptyMVar
    withFakeAMIUsing (answeredFake active) $ \requests -> withOwner nativeMedia (checkAnswered active requests)

withOwner :: Native.TelephonyMedia -> (Native.OwnerControl -> IO ()) -> IO ()
withOwner media action = do
  opened <- Native.openOwner nativeConfig Right media "call"
  either (expectationFailure . show) (\control -> bracket (pure control) Native.closeOwner action) opened

directArgs :: Value
directArgs = object ["number" .= ("+442079460018" :: Text)]

nativeRedirects :: MVar [A.Message] -> IO [A.Message]
nativeRedirects requests = filter isDial <$> readMVar requests
  where
    isDial request = A.field "Action" request == "Redirect" && A.field "Context" request == "phone-assistant-direct-call"

checkDisconnected :: MVar [A.Message] -> Native.OwnerControl -> IO ()
checkDisconnected requests control = do
  result <- timeout 3000000 (Native.executeDirect control "direct_call" directArgs "disconnect")
  fmap (fmap (objectField "reason")) result `shouldBe` Just (Right (String "call_control_disconnected"))
  actions <- readMVar requests
  length [request | request <- actions, A.field "Action" request == "Login"] `shouldBe` 2
  length <$> nativeRedirects requests `shouldReturn` 1
  length [request | request <- actions, A.field "Context" request == "phone-assistant-resume"] `shouldBe` 1

checkRecoveryFailure :: MVar [A.Message] -> Native.OwnerControl -> IO ()
checkRecoveryFailure requests control = do
  first <- Native.executeDirect control "direct_call" directArgs "failed"
  fmap (objectField "result") first `shouldBe` Right (String "cancelled")
  Native.ownerIsEnded control `shouldReturn` True
  second <- Native.executeDirect control "direct_call" directArgs "another"
  fmap (objectField "result") second `shouldBe` Right (String "cancelled")
  length <$> nativeRedirects requests `shouldReturn` 1

checkAnswered :: MVar Socket -> MVar [A.Message] -> Native.OwnerControl -> IO ()
checkAnswered active requests control = do
  first <- Native.executeDirect control "direct_call" directArgs "answered"
  fmap (objectField "result") first `shouldBe` Right (String "connected")
  Native.ownerAnswered control `shouldReturn` True
  timeout 20000 (Native.waitOwnerFinished control) `shouldReturn` Nothing
  actions <- readMVar requests
  [request | request <- actions, A.field "Action" request == "Hangup"] `shouldBe` []
  connection <- takeMVar active
  sendFields connection [("Event", "Hangup"), ("Uniqueid", "123.4")]
  timeout 1000000 (Native.waitOwnerFinished control) `shouldReturn` Just (Right ())
  Native.ownerIsEnded control `shouldReturn` True
  length <$> nativeRedirects requests `shouldReturn` 1

answeredFake :: MVar Socket -> Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()
answeredFake active connection variables message
  | A.field "Context" message == "phone-assistant-direct-call" = do
      sendFields connection [("Response", "Success"), ("ActionID", A.field "ActionID" message)]
      values <- readMVar variables
      let attempt = M.findWithDefault "" "PA_DIRECT_ATTEMPT" values
      sendFields connection [("Event", "UserEvent"), ("UserEvent", "PhoneAssistantDialAnswered"), ("CallID", "call"), ("AttemptID", attempt)]
      putMVar active connection
  | otherwise = respondFake connection variables message

disconnectedFake :: Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()
disconnectedFake connection variables message
  | A.field "Context" message == "phone-assistant-direct-call" = do
      sendFields connection [("Response", "Success"), ("ActionID", A.field "ActionID" message)]
      shutdown connection ShutdownBoth
  | A.field "Context" message == "phone-assistant-resume" = sendFields connection [("Response", "Success"), ("ActionID", A.field "ActionID" message)]
  | otherwise = respondFake connection variables message


checkNoDial :: MVar [A.Message] -> Native.TelephonyMedia -> Text -> IO ()
checkNoDial requests media expectedReason = do
  opened <- Native.openOwner nativeConfig Right media "call"
  case opened of
    Left problem -> expectationFailure (show problem)
    Right control -> bracket (pure control) Native.closeOwner run
  where
    run :: Native.OwnerControl -> IO ()
    run control = do
      result <- Native.executeDirect control "direct_call" (object ["number" .= ("+442079460018" :: Text)]) "request"
      let expectedResult | T.null expectedReason = "failed"
                         | otherwise = "cancelled"
      fmap (objectField "result") result `shouldBe` Right (String expectedResult)
      unless (T.null expectedReason) (fmap (objectField "reason") result `shouldBe` Right (String expectedReason))
      actions <- readMVar requests
      filter (\request -> A.field "Action" request == "Redirect") actions `shouldBe` []

revokedFake :: Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()
revokedFake connection variables message
  | A.field "Action" message == "Getvar" && A.field "Variable" message == "PA_OWNER_AUTHORIZED" =
      sendFields connection [("Response", "Success"), ("ActionID", A.field "ActionID" message), ("Value", "0")]
  | otherwise = respondFake connection variables message

nativeConfig :: Native.TelephonyConfig
nativeConfig = Native.TelephonyConfig "test" "test" True (RouteSettings True True True) 2 "/tmp" nativeRules amiTestRules

nativeMedia :: Native.TelephonyMedia
nativeMedia = Native.TelephonyMedia (\_ _ -> pure (Right True)) (pure ()) (pure ()) (\_ -> pure (Right ())) (const (pure ()))

exerciseNative :: MVar [A.Message] -> Native.OwnerControl -> IO ()
exerciseNative requests control = do
  let args = object ["number" .= ("+442079460018" :: Text), "display_name" .= (Nothing :: Maybe Text)]
  first <- Native.executeDirect control "direct_call" args "request-one"
  second <- Native.executeDirect control "direct_call" args "request-one"
  first `shouldBe` second
  let expected = object ["status" .= ("ok" :: Text), "result" .= ("busy" :: Text), "cause" .= (17 :: Int), "sip_status" .= (486 :: Int), "number" .= ("+442079460018" :: Text), "provider" .= ("aa" :: Text), "display_name" .= (Nothing :: Maybe Text)]
  received <- readMVar requests
  first `shouldBe` Right expected
  let redirects = filter (\message -> A.field "Action" message == "Redirect" && A.field "Context" message == "phone-assistant-direct-call") received
  length redirects `shouldBe` 1

withFakeAMI :: (MVar [A.Message] -> IO ()) -> IO ()
withFakeAMI = withFakeAMIUsing respondFake

withFakeAMIUsing :: (Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()) -> (MVar [A.Message] -> IO ()) -> IO ()
withFakeAMIUsing respond action = bracket makeListener close run
  where
    makeListener :: IO Socket
    makeListener = do
      listener <- socket AF_INET Stream defaultProtocol
      setSocketOption listener ReuseAddr 1
      bind listener (SockAddrInet 5038 (tupleToHostAddress (127,0,0,1)))
      listen listener 1
      pure listener
    run :: Socket -> IO ()
    run listener = do
      requests <- newMVar []
      withAsync (forever (fakeClient respond listener requests)) (const (action requests))

fakeClient :: (Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()) -> Socket -> MVar [A.Message] -> IO ()
fakeClient respond listener requests = bracket (fst <$> accept listener) close run
  where
    run :: Socket -> IO ()
    run connection = do
      N.sendAll connection "Asterisk Call Manager/5.0\r\n"
      values <- newMVar M.empty
      fakeLoop respond connection requests values

fakeLoop :: (Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()) -> Socket -> MVar [A.Message] -> MVar (M.Map Text Text) -> IO ()
fakeLoop respond connection requests variables = do
  received <- readFakeMessage connection
  case received of
    Left _ -> pure ()
    Right message -> do
      modifyMVar_ requests (pure . (message :))
      respond connection variables message
      fakeLoop respond connection requests variables

sendFields :: Socket -> [(Text, Text)] -> IO ()
sendFields connection fields = N.sendAll connection (encodeUtf8 rendered)
  where
    rendered :: Text
    rendered = T.intercalate "\r\n" (map (\(key, value) -> key <> ": " <> value) fields) <> "\r\n\r\n"

respondFake :: Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()
respondFake connection values message = case A.field "Action" message of
  "Status" -> fakeStatus connection identifier
  "Getvar" -> sendFields connection [("Response", "Success"), ("ActionID", identifier), ("Value", variableValue (A.field "Variable" message))]
  "Setvar" -> modifyMVar_ values (pure . M.insert (A.field "Variable" message) (A.field "Value" message)) >> success
  "Redirect" -> success >> fakeDialResult connection values
  _ -> success
  where
    identifier :: Text
    identifier = A.field "ActionID" message
    success :: IO ()
    success = sendFields connection [("Response", "Success"), ("ActionID", identifier)]
    variableValue :: Text -> Text
    variableValue "PA_CALL_ID" = "call"
    variableValue "PA_OWNER_AUTHORIZED" = "1"
    variableValue "PA_PERMISSION" = "owner"
    variableValue _ = ""

fakeStatus :: Socket -> Text -> IO ()
fakeStatus connection identifier = do
  let base = M.toList (A.messageFields (status "owner" "1" "phone-assistant-inbound" ""))
  let vars = [("Variable", "PA_CALL_ID=call"), ("Variable", "PA_PERMISSION=owner"), ("Variable", "PA_OWNER_AUTHORIZED=1")]
  sendFields connection (("ActionID", identifier) : base ++ vars)
  sendFields connection [("ActionID", identifier), ("Event", "StatusComplete")]

fakeDialResult :: Socket -> MVar (M.Map Text Text) -> IO ()
fakeDialResult connection values = do
  variables <- readMVar values
  let attempt = M.findWithDefault "" "PA_DIRECT_ATTEMPT" variables
  sendFields connection [("Event", "UserEvent"), ("UserEvent", "PhoneAssistantDialResult"), ("CallID", "call"), ("AttemptID", attempt), ("Result", "BUSY"), ("Cause", "17"), ("SIPCause", "SIP 486 Busy")]

readFakeMessage :: Socket -> IO (Result A.Message)
readFakeMessage connection = collect B.empty
  where
    collect :: B.ByteString -> IO (Result A.Message)
    collect bytes
      | "\r\n\r\n" `B.isSuffixOf` bytes = pure (Right (preserveVariable bytes))
      | otherwise = N.recv connection 1 >>= append bytes
    append :: B.ByteString -> B.ByteString -> IO (Result A.Message)
    append bytes chunk
      | B.null chunk = pure (Left (failure "closed"))
      | otherwise = collect (bytes <> chunk)
    preserveVariable :: B.ByteString -> A.Message
    preserveVariable bytes = foldl add (A.parseMessage bytes) (T.splitOn "\r\n" (decodeUtf8 bytes))
    add :: A.Message -> Text -> A.Message
    add message line = case T.stripPrefix "Variable: " line of
      Nothing -> message
      Just value -> message {A.messageFields = M.insert "Variable" value (A.messageFields message)}

withPublicSettings :: (Settings -> Expectation) -> Expectation
withPublicSettings check = do
  bytes <- B.readFile "config/global.json"
  case eitherDecodeStrict' bytes of
    Left problem -> expectationFailure problem
    Right global -> check (resolveSettings (global :: Global) (User M.empty))

publicSpec :: Spec
publicSpec = do
  it "builds a public session from restricted instructions and tools only" $ withPublicSettings $ \settings -> do
    let poisoned = settings {settingsValues = M.insert "PHONE_ASSISTANT_LIVE_INSTRUCTIONS" "PRIVATE OWNER SENTINEL" (settingsValues settings)}
    case restrictedSettings poisoned False False of
      Left problem -> expectationFailure (show problem)
      Right cfg -> do
        let session = liveSession cfg
            backend = objectField "responses" (objectField "delegation" session)
        objectField "instructions" backend `shouldBe` objectField "instructions" session
        objectField "max_output_tokens" backend `shouldBe` Number 256
        toolsNamed (objectField "tools" backend) `shouldBe` ["screen_call"]
        B.isInfixOf "PRIVATE OWNER SENTINEL" (BL.toStrict (encode session)) `shouldBe` False
  it "permits messages but removes forwarding in a message-only session" $ withPublicSettings $ \settings ->
    case restrictedSettings settings True True of
      Left problem -> expectationFailure (show problem)
      Right cfg -> do
        let backend = objectField "responses" (objectField "delegation" (liveSession cfg))
        toolsNamed (objectField "tools" backend) `shouldBe` ["screen_call", "take_message"]
        objectField "max_output_tokens" backend `shouldBe` Number 1024
        B.isInfixOf "\"enum\":[\"decline\"]" (BL.toStrict (encode (objectField "tools" backend))) `shouldBe` True
  it "cleans handoff facts while keeping known and unknown caller wording exact" $ do
    let rules = HandoffRules "I have {who} for you." "a caller" "a caller named {name}" " They said: {detail}." ["unknown", "name not provided"]
    handoffLine rules "Bob\NUL Smith" "A delivery..." False `shouldBe` "I have a caller named Bob Smith for you. They said: A delivery."
    handoffLine rules "Bob Smith" "Ignored detail" True `shouldBe` "I have Bob Smith for you."
    handoffLine rules "unknown" "" False `shouldBe` "I have a caller for you."

objectField :: Data.Aeson.Key.Key -> Value -> Value
objectField key (Object fields) = fromMaybe Null (KM.lookup key fields)
objectField _ _ = Null

toolsNamed :: Value -> [Text]
toolsNamed (Array values) = [name | Object fields <- Data.Vector.toList values, Just (String name) <- [KM.lookup "name" fields]]
toolsNamed _ = []

managedSpec :: Spec
managedSpec = do
  it "expires paused media and rejects a late replacement socket" $ withSocketPairs $ \first second -> do
    registry <- newRegistry socketTestRules 20000
    result <- withManaged registry "expired" first $ \managed -> do
      pauseManaged managed
      waitManaged managed 1 `shouldReturn` Left (failure "call_resume_timeout")
      receiveAudio (managedMedia managed) `shouldReturn` Left (failure "call_resume_timeout")
      withManaged registry "expired" second (const (pure (Right ())))
    result `shouldBe` Left (failure "call_already_attached")
  it "cancels the previous pause deadline when the existing socket resumes" $ withSocketPairs $ \first _ -> do
    registry <- newRegistry socketTestRules 20000
    result <- withManaged registry "resumed" first $ \managed -> do
      pauseManaged managed
      resumeManaged managed
      pauseManaged managed
      waitManaged managed 1 `shouldReturn` Left (failure "call_resume_timeout")
      pure (Right ())
    result `shouldBe` Right ()
  it "invalidates the old pause timer after a replacement socket attaches" $ withSocketPairs $ \first second -> do
    registry <- newRegistry socketTestRules 20000
    result <- withManaged registry "reattached" first $ \managed -> do
      pauseManaged managed
      withAsync (withManaged registry "reattached" second (const (pure (Right ())))) $ \_ -> do
        waitManaged managed 1 `shouldReturn` Right ()
        timeout 60000 (receiveAudio (managedMedia managed)) `shouldReturn` Nothing
      pure (Right ())
    result `shouldBe` Right ()

  it "rejects reattachment after native ownership leaves the assistant" $ withSocketPairs $ \first second -> do
    registry <- newRegistry socketTestRules 30000000
    result <- withManaged registry "same-call" first $ \managed -> do
      pauseManaged managed
      setReattachAllowed managed (pure False)
      withManaged registry "same-call" second (const (pure (Right ())))
    result `shouldBe` Left (failure "call_has_left_assistant")
  it "keeps equal public and owner UUIDs in separate ingress registries" $ withSocketPairs $ \first second -> do
    ownerRegistry <- newRegistry socketTestRules 30000000
    publicRegistry <- newRegistry socketTestRules 30000000
    result <- withManaged ownerRegistry "same-call" first $ \managed -> do
      pauseManaged managed
      withManaged publicRegistry "same-call" second (const (pure (Left (failure "incoming_call_not_owned"))))
    result `shouldBe` Left (failure "incoming_call_not_owned")

withSocketPairs :: (Socket -> Socket -> IO ()) -> IO ()
withSocketPairs action = bracket create closePair $ \(first, _) ->
  bracket create closePair (\(second, _) -> action first second)
  where
    create :: IO (Socket, Socket)
    create = socketPair AF_UNIX Stream defaultProtocol
    closePair :: (Socket, Socket) -> IO ()
    closePair (first, second) = close first >> close second

nativeRules :: Native.TelephonyRules
nativeRules = Native.TelephonyRules 3 12000000 8 5 5000000 1024 48 8192 128 500 120 160

amiTestRules :: A.AMIRules
amiTestRules = A.AMIRules 5038 4000000 3000000 3000000 4000000 65536 1024 512 256

socketTestRules :: SocketRules
socketTestRules = SocketRules 5000000 5000000

unansweredStatus :: Socket -> MVar (M.Map Text Text) -> A.Message -> IO ()
unansweredStatus connection variables message
  | A.field "Action" message == "Status" = pure ()
  | otherwise = respondFake connection variables message
