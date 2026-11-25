module Baerbel.Core.AMI where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( jsonOptions )
import Data.Aeson ( FromJSON(..), genericParseJSON )
import Data.ByteString ( ByteString )
import Data.Char ( isAscii, isAlpha, isAlphaNum, isHexDigit, ord )
import Data.Map.Strict ( Map )
import Data.Maybe ( isJust, isNothing )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8With, encodeUtf8 )
import Data.Text.Encoding.Error ( lenientDecode )
import GHC.Generics ( Generic )
import qualified Data.List as List ( foldl' )
import qualified Data.Map.Strict as M
    ( insert, empty, findWithDefault )
import qualified Data.Text as T
    ( all,
      breakOn,
      drop,
      intercalate,
      length,
      null,
      splitOn,
      stripPrefix,
      stripStart,
      uncons )

data AMIRules = AMIRules
  { amiPort :: Int, amiConnectMicros :: Int, amiBannerMicros :: Int
  , amiWriteMicros :: Int, amiActionMicros :: Int, amiMessageBytes :: Int
  , amiFieldChars :: Int, amiEventLimit :: Int, amiChannelChars :: Int
  } deriving (Eq, Show, Generic)
instance FromJSON AMIRules where parseJSON = genericParseJSON (jsonOptions 3)

data Message = Message
  { messageFields :: Map Text Text
  , messageVariables :: Map Text Text
  } deriving (Eq, Show)

data Channel = Channel
  { channelName :: Text
  , channelUniqueId :: Text
  , channelMessageOnly :: Bool
  } deriving (Eq, Show)

field :: Text -> Message -> Text
field key = M.findWithDefault "" key . messageFields

variable :: Text -> Message -> Text
variable key = M.findWithDefault "" key . messageVariables

parseMessage :: ByteString -> Message
parseMessage bytes = List.foldl' addLine (Message M.empty M.empty) rows
  where
    rows :: [Text]
    rows = T.splitOn "\r\n" (decodeUtf8With lenientDecode bytes)

addLine :: Message -> Text -> Message
addLine message line
  | T.null rest = message
  | key == "Variable" = addVariable message value
  | otherwise = message { messageFields = M.insert key value (messageFields message) }
  where
    key :: Text
    rest :: Text
    (key, rest) = T.breakOn ":" line
    value :: Text
    value = T.stripStart (T.drop 1 rest)

addVariable :: Message -> Text -> Message
addVariable message value
  | T.null rest = message
  | otherwise = message { messageVariables = M.insert name content (messageVariables message) }
  where
    name :: Text
    rest :: Text
    (name, rest) = T.breakOn "=" value
    content :: Text
    content = T.drop 1 rest

validField :: AMIRules -> (Text, Text) -> Bool
validField rules (key, value) = nameValid && T.length value <= amiFieldChars rules && T.all visible value
  where
    nameValid :: Bool
    nameValid = case T.uncons key of
      Nothing -> False
      Just (first, rest) -> isAscii first && isAlpha first && T.all nameChar rest
    nameChar :: Char -> Bool
    nameChar char = isAscii char && isAlphaNum char
    visible :: Char -> Bool
    visible char = ord char >= 32 && ord char /= 127

encodeAction :: AMIRules -> Text -> Text -> [(Text, Text)] -> Result ByteString
encodeAction rules action identifier fields
  | all (validField rules) pairs = Right (encodeUtf8 body)
  | otherwise = Left (failure "ami_invalid_action")
  where
    pairs :: [(Text, Text)]
    pairs = [("Action", action), ("ActionID", identifier)] ++ fields
    body :: Text
    body = T.intercalate "\r\n" (map (\(key, value) -> key <> ": " <> value) pairs) <> "\r\n\r\n"

actionResponse :: AMIRules -> Maybe Text -> [Message] -> Message -> Result (Maybe [Message])
actionResponse rules complete previous message
  | field "Response" message == "Error" = Left (failure "ami_action_rejected")
  | Just (field "Event" message) == complete = Right (Just (reverse previous))
  | isJust complete && not (T.null (field "Event" message)) && length previous >= amiEventLimit rules = Left (failure "ami_event_limit")
  | isNothing complete && not (T.null (field "Response" message)) = Right (Just [message])
  | otherwise = Right Nothing

ownerChannel :: AMIRules -> Text -> [Message] -> Result Channel
ownerChannel rules identifier messages = selectChannel "ami_call_not_found" "ami_invalid_channel" validName matches
  where
    matches :: [Message]
    matches = filter (ownerMatch identifier) messages
    validName :: Text -> Bool
    validName name = not (T.null name) && T.length name <= amiChannelChars rules && T.all allowed name
    allowed :: Char -> Bool
    allowed char = isAscii char && (isAlphaNum char || char `elem` ("_./:@;+-" :: String))

ownerMatch :: Text -> Message -> Bool
ownerMatch identifier message = field "Event" message == "Status"
  && variable "PA_CALL_ID" message == identifier && variable "PA_OWNER_AUTHORIZED" message == "1"
  && variable "PA_PERMISSION" message == "owner" && field "Application" message == "AudioSocket"
  && field "Context" message `elem` ["phone-assistant-inbound", "phone-assistant-resume"]

incomingChannel :: Text -> [Message] -> Result Channel
incomingChannel identifier messages = selectChannel "incoming_call_not_found" "incoming_invalid_channel" incomingName matches
  where
    matches :: [Message]
    matches = filter (incomingMatch identifier) messages

incomingMatch :: Text -> Message -> Bool
incomingMatch identifier message = field "Event" message == "Status"
  && variable "PA_CALL_ID" message == identifier && variable "PA_PERMISSION" message == "screening"
  && variable "PA_OWNER_AUTHORIZED" message == "0" && field "Application" message == "AudioSocket"
  && (field "Context" message, variable "PA_INCOMING_MODE" message)
     `elem` [("phone-assistant-screening", "screening"), ("phone-assistant-message", "message")]

incomingName :: Text -> Bool
incomingName name = any matches ["PJSIP/phone-assistant-aa-", "PJSIP/phone-assistant-telematica-"]
  where
    matches :: Text -> Bool
    matches prefix = case T.stripPrefix prefix name of
      Nothing -> False
      Just suffix -> not (T.null suffix) && T.all isHexDigit suffix

selectChannel :: Text -> Text -> (Text -> Bool) -> [Message] -> Result Channel
selectChannel missing invalid permitted messages = case messages of
  [message] | permitted (field "Channel" message) && not (T.null (field "Uniqueid" message)) ->
    Right (Channel (field "Channel" message) (field "Uniqueid" message) (field "Context" message == "phone-assistant-message"))
  [_] -> Left (failure invalid)
  _ -> Left (failure missing)
