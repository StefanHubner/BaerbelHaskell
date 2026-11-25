{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Telephony where

import Baerbel.Core.AMI (AMIRules, Channel(..), Message, field, variable)
import Baerbel.Core.CallState
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (jsonOptions, resultValue, textField)
import Baerbel.Core.Routing
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Char (isAscii, isDigit, isHexDigit, ord)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)

data TelephonyConfig = TelephonyConfig
  { amiUsername :: Text, amiPassword :: Text, directEnabled :: Bool
  , routeSettings :: RouteSettings, dialTimeoutSeconds :: Int, recordingDirectory :: FilePath, telephonyRules :: TelephonyRules, amiRules :: AMIRules }

data TelephonyRules = TelephonyRules
  { nativeDialGraceSeconds :: Int, nativeRecoveryMicros :: Int
  , nativeRecoveryResumeSeconds :: Int, nativeResultResumeSeconds :: Int
  , nativeAGIMicros :: Int, nativeAGILineBytes :: Int, nativeAGIHeaderCount :: Int
  , nativeAGIHeaderBytes :: Int, nativeHandoffLimit :: Int
  , nativeHandoffLineChars :: Int, nativeHandoffSeconds :: Double
  , nativeLabelChars :: Int } deriving (Generic)
instance FromJSON TelephonyRules where parseJSON = genericParseJSON (jsonOptions 6)

data IncomingEnd = Declined | ScreeningTimeout | ScreeningError | MessageSent | MessageFailed | MessageUncertain

data DirectMode = InspectRoute | DialNumber deriving (Eq)

data DirectArgs = DirectArgs
  { argumentNumber :: Text, argumentLabel :: Maybe Text
  , argumentToken :: Maybe Text, argumentConfirmed :: Bool }

textArgument :: Text -> KM.KeyMap Value -> Result Text
textArgument key fields = maybe invalid Right (textField key (Object fields))
  where invalid = Left (failure "invalid_arguments")

nullableText :: Text -> KM.KeyMap Value -> Result (Maybe Text)
nullableText key fields = case KM.lookup (K.fromText key) fields of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (String value) -> Right (Just value)
  _ -> Left (failure "invalid_arguments")

parseDirect :: TelephonyRules -> Text -> Value -> Result (DirectMode, DirectArgs)
parseDirect rules name arguments = do
  mode <- directMode name
  args <- directArguments rules mode arguments
  pure (mode, args)

directMode :: Text -> Result DirectMode
directMode "direct_call" = Right DialNumber
directMode "direct_call_route" = Right InspectRoute
directMode _ = Left (failure "invalid_arguments")

directArguments :: TelephonyRules -> DirectMode -> Value -> Result DirectArgs
directArguments rules mode (Object fields)
  | any (`notElem` allowed) (KM.keys fields) = Left (failure "invalid_arguments")
  | otherwise = DirectArgs <$> directNumber mode fields <*> label <*> token <*> confirmation
  where
    allowed :: [K.Key]
    allowed = ["number", "display_name"] ++ [key | mode == DialNumber, key <- ["route_token", "international_confirmed"]]
    label :: Result (Maybe Text)
    label = nullableText "display_name" fields >>= checkLabel rules
    token :: Result (Maybe Text)
    token = nullableText "route_token" fields >>= checkToken
    confirmation :: Result Bool
    confirmation = case KM.lookup "international_confirmed" fields of
      Nothing -> Right False
      Just (Bool value) -> Right value
      _ -> Left (failure "invalid_arguments")
directArguments _ _ _ = Left (failure "invalid_arguments")

checkLabel :: TelephonyRules -> Maybe Text -> Result (Maybe Text)
checkLabel rules value
  | maybe True (\text -> T.length text <= nativeLabelChars rules && T.all (\char -> ord char >= 32 && ord char /= 127) text) value = Right value
  | otherwise = Left (failure "invalid_display_name")

checkToken :: Maybe Text -> Result (Maybe Text)
checkToken value
  | maybe True (\text -> T.length text == 32 && T.all (\char -> char `elem` ("0123456789abcdef" :: String)) text) value = Right value
  | otherwise = Left (failure "invalid_arguments")

sendDigit :: Message -> CallState -> Maybe ByteString
sendDigit message state
  | field "Event" message == "DTMFEnd" && matches && permitted = Just (encodeUtf8 digit)
  | otherwise = Nothing
  where
    matches :: Bool
    matches = field "Channel" message == channelName (callChannel state)
      && field "Uniqueid" message == channelUniqueId (callChannel state)
    digit :: Text
    digit = field "Digit" message
    permitted :: Bool
    permitted = callPhase state == Conversing && field "Direction" message == "Received"
      && T.length digit == 1 && T.all (`elem` ("0123456789*#" :: String)) digit

verifyHome :: Route -> Channel -> Result ()
verifyHome route channel
  | routeProvider route == Home && T.isPrefixOf "PJSIP/phone-assistant-home-" (channelName channel) = Left (failure "already_on_home_phone")
  | otherwise = Right ()

markDialing :: Maybe CallState -> (Maybe CallState, Result ())
markDialing (Just state) | callPhase state == Announcing = (Just state {callPhase = Dialing}, Right ())
markDialing state = (state, Left (failure "call_cancelled"))

dialVariables :: Int -> DialRequest -> [(Text, Text)]
dialVariables duration request = [("PA_DIRECT_NUMBER", routeNumber route), ("PA_DIRECT_PROVIDER", provider),
  ("PA_DIRECT_ATTEMPT", dialAttemptId request), ("PA_DIRECT_TIMEOUT", T.pack (show duration))]
  where
    route :: Route
    route = dialRoute request
    provider :: Text
    provider = providerName (routeProvider route)

data PendingHandoff = PendingHandoff
  { handoffSource :: Channel, handoffCallId :: Text, handoffText :: Text, handoffDeadline :: Double }
ownerEndpoint :: Channel -> Text -> Bool
ownerEndpoint channel endpoint = endpoint `elem` ["phone-assistant-aa-outbound", "phone-assistant-home"]
  && T.isPrefixOf ("PJSIP/" <> endpoint <> "-") (channelName channel)

handoffChannel :: Text -> PendingHandoff -> [Message] -> Result Channel
handoffChannel token pending messages = case (filter source messages, filter destination messages) of
  ([_], [owner]) -> Right (Channel (field "Channel" owner) (field "Uniqueid" owner) False)
  _ -> Left (failure "incoming_handoff_not_owned")
  where
    source :: Message -> Bool
    source message = field "Channel" message == channelName (handoffSource pending)
      && field "Uniqueid" message == channelUniqueId (handoffSource pending)
      && field "Event" message == "Status" && field "Context" message == "phone-assistant-public-forward"
      && field "Application" message == "Dial" && variable "PA_CALL_ID" message == handoffCallId pending
      && variable "PA_HANDOFF_ID" message == token && variable "PA_PERMISSION" message == "call_only"
      && variable "PA_OWNER_AUTHORIZED" message == "0" && variable "PA_INCOMING_MODE" message == "screening"
    destination :: Message -> Bool
    destination message = field "Event" message == "Status" && field "Context" message == "phone-assistant-public-answered"
      && field "Application" message == "AudioSocket" && field "ChannelStateDesc" message == "Up"
      && T.null (field "BridgeID" message) && variable "PA_CALL_ID" message == token
      && variable "PA_HANDOFF_PARENT" message == channelUniqueId (handoffSource pending)
      && variable "PA_PERMISSION" message == "handoff" && variable "PA_OWNER_AUTHORIZED" message == "0"
      && validOwnerChannel (field "Channel" message)

validOwnerChannel :: Text -> Bool
validOwnerChannel name = any matches ["PJSIP/phone-assistant-aa-outbound-", "PJSIP/phone-assistant-home-"]
  where
    matches :: Text -> Bool
    matches prefix = case T.stripPrefix prefix name of
      Just suffix -> not (T.null suffix) && T.all isHexDigit suffix
      Nothing -> False

incomingReason :: IncomingEnd -> Text
incomingReason Declined = "DECLINED"
incomingReason ScreeningTimeout = "SCREENING_TIMEOUT"
incomingReason ScreeningError = "SCREENING_ERROR"
incomingReason MessageSent = "MESSAGE_SENT"
incomingReason MessageFailed = "MESSAGE_FAILED"
incomingReason MessageUncertain = "MESSAGE_UNCERTAIN"

homeIdentity :: TelephonyConfig -> M.Map Text Text -> Result Text
homeIdentity cfg headers
  | not (homeEnabled (routeSettings cfg)) || headerValue "agi_network_script" /= "home-dial"
      || headerValue "agi_context" /= "phone-assistant-home-inbound" || not validChannel = Left (failure "home_channel_required")
  | not validNumber = Left (failure "invalid_home_destination")
  | domestic = Right ("+44" <> T.drop 1 destination)
  | otherwise = Right destination
  where
    headerValue :: Text -> Text
    headerValue key = M.findWithDefault "" key headers
    validChannel :: Bool
    validChannel = case T.stripPrefix "PJSIP/phone-assistant-home-" (headerValue "agi_channel") of
      Nothing -> False
      Just suffix -> not (T.null suffix) && T.all isHexDigit suffix
    destination :: Text
    destination = headerValue "agi_extension"
    digits :: Text
    digits = fromMaybe destination (T.stripPrefix "+" destination)
    validNumber :: Bool
    validNumber = T.length digits >= 3 && T.length digits <= 17 && T.all (\char -> isAscii char && isDigit char) digits
    domestic :: Bool
    domestic = T.isPrefixOf "0" destination && not (T.isPrefixOf "00" destination) && T.length destination `elem` [10,11]

homeChecks :: [(Text, Text)]
homeChecks = [("GET FULL VARIABLE \"${CHANNEL(endpoint)}\"", "200 result=1 (phone-assistant-home)"),
  ("GET VARIABLE PA_OWNER_AUTHORIZED", "200 result=1 (1)"), ("GET VARIABLE PA_PERMISSION", "200 result=1 (owner)")]

homeCommands :: Route -> [(Text, Text)]
homeCommands route = map command [("PA_DIRECT_NUMBER", routeNumber route), ("PA_DIRECT_PROVIDER", provider), ("PA_HOME_DIAL_READY", "1")]
  where
    provider :: Text
    provider = providerName (routeProvider route)
    command :: (Text, Text) -> (Text, Text)
    command (key, value) = ("SET VARIABLE " <> key <> " \"" <> value <> "\"", "200 result=1")

completedOutcome :: Text -> Value -> CallState -> CallState
completedOutcome identifier result state = state {callPhase = phase, callResults = results}
  where
    results = M.insert identifier result (callResults state)
    phase | callPhase state `elem` [Ended, Connected] = callPhase state
          | otherwise = Conversing

failedDial :: DialRequest -> CallState -> Value
failedDial request state = addDestination request (resultValue result)
  where
    result | callPhase state == Connected = "connected"
           | callPhase state == Ended = "cancelled"
           | otherwise = "failed"

addDestination :: DialRequest -> Value -> Value
addDestination request (Object fields) = Object (KM.union fields additions)
  where
    provider :: Text
    provider = providerName (routeProvider (dialRoute request))
    additions :: KM.KeyMap Value
    additions = KM.fromList [("number", String (routeNumber (dialRoute request))), ("provider", String provider), ("display_name", maybe Null String (dialLabel request))]
addDestination _ value = value

cachedCommand :: Bool -> Text -> CallState -> Maybe (Result Value)
cachedCommand enabled identifier state
  | Just result <- M.lookup identifier (callResults state) = Just (Right result)
  | callPhase state == Ended = Just (Right (resultValue "cancelled"))
  | callPhase state == Connected = Just (Right (resultValue "connected"))
  | not enabled = Just (Left (failure "direct_calls_disabled"))
  | otherwise = Nothing

directNumber :: DirectMode -> KM.KeyMap Value -> Result Text
directNumber mode fields = textArgument "number" fields >>= canonical
  where
    canonical :: Text -> Result Text
    canonical number
      | mode == InspectRoute || number == "home" || normalizeDestination number == Right number = Right number
      | otherwise = Left (failure "invalid_e164_number")
