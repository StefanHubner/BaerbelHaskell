{-# LANGUAGE OverloadedStrings #-}
module Baerbel.AMI
  ( AMIRules(..), Message(..), Channel(..), AMI(..), parseMessage, encodeAction
  , readMessage, actionResponse, connectAMI, ownerChannel, incomingChannel
  , field, variable
  ) where

import Baerbel.Concurrent (stateMVar, updateMVar)
import Baerbel.Effects (attempt, continue, within)
import Baerbel.JSON (jsonOptions)
import Data.Aeson (FromJSON(..), genericParseJSON)
import GHC.Generics (Generic)
import Baerbel.Error (Failure(..), Result, failure, ioResult)
import Control.Concurrent (MVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracketOnError, finally, onException)
import Control.Monad (void, unless)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Maybe (isJust, isNothing)
import Data.Char (isAscii, isAlpha, isAlphaNum, isHexDigit, ord)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.Socket (Socket, Family(AF_INET), SocketType(Stream), SockAddr(SockAddrInet), close, connect, defaultProtocol, socket, tupleToHostAddress)
import qualified Network.Socket.ByteString as N

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

data AMI = AMI
  { amiAction :: Text -> [(Text, Text)] -> Maybe Text -> IO (Result [Message])
  , amiClose :: IO ()
  }

data Pending = Pending
  { pendingComplete :: Maybe Text
  , pendingEvents :: [Message]
  , pendingResult :: MVar (Result [Message])
  }

type Requests = MVar (Map Text Pending)

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

readDelimited :: Socket -> ByteString -> Int -> IO (Result ByteString)
readDelimited connection delimiter limit = readNext connection delimiter limit [] B.empty 0

readNext :: Socket -> ByteString -> Int -> [ByteString] -> ByteString -> Int -> IO (Result ByteString)
readNext connection delimiter limit chunks ending size
  | ending == delimiter = pure (Right (B.concat (reverse chunks)))
  | size >= limit = pure (Left (failure "ami_event_limit"))
  | otherwise = do
    byte <- N.recv connection 1
    let next = B.takeEnd (B.length delimiter) (ending <> byte)
    case B.uncons byte of
      Nothing -> pure (Left (failure "ami_disconnected"))
      Just _ -> readNext connection delimiter limit (byte : chunks) next (size + 1)

readMessage :: AMIRules -> Socket -> IO (Result Message)
readMessage rules connection = fmap parseMessage <$> attempt "ami_disconnected" readRaw
  where
    bound = amiMessageBytes rules + 4
    readRaw = readDelimited connection "\r\n\r\n" bound

actionResponse :: AMIRules -> Maybe Text -> [Message] -> Message -> Result (Maybe [Message])
actionResponse rules complete previous message
  | field "Response" message == "Error" = Left (failure "ami_action_rejected")
  | Just (field "Event" message) == complete = Right (Just (reverse previous))
  | isJust complete && not (T.null (field "Event" message)) && length previous >= amiEventLimit rules = Left (failure "ami_event_limit")
  | isNothing complete && not (T.null (field "Response" message)) = Right (Just [message])
  | otherwise = Right Nothing

data ActionProgress = ActionPending Pending | ActionFinished (Result [Message])

advance :: AMIRules -> Message -> Pending -> ActionProgress
advance rules message pending = case actionResponse rules (pendingComplete pending) (pendingEvents pending) message of
  Left problem -> ActionFinished (Left problem)
  Right (Just result) -> ActionFinished (Right result)
  Right Nothing -> ActionPending (appendEvent message pending)

appendEvent :: Message -> Pending -> Pending
appendEvent message pending
  | isNothing (pendingComplete pending) || T.null (field "Event" message) = pending
  | otherwise = pending { pendingEvents = message : pendingEvents pending }

finish :: Pending -> Result [Message] -> IO ()
finish pending result = void (tryPutMVar (pendingResult pending) result)

notify :: AMIRules -> Message -> Map Text Pending -> (Map Text Pending, Maybe (Pending, Result [Message]))
notify rules message pending = case M.lookup identifier pending of
  Nothing -> (pending, Nothing)
  Just requestValue -> case advance rules message requestValue of
    ActionPending value -> (M.insert identifier value pending, Nothing)
    ActionFinished result -> (M.delete identifier pending, Just (requestValue, result))
  where identifier = field "ActionID" message

disconnect :: Requests -> IO ()
disconnect requests = modifyMVar_ requests clear
  where
    clear :: Map Text Pending -> IO (Map Text Pending)
    clear pending = mapM_ failRequest (M.elems pending) >> pure M.empty
    failRequest :: Pending -> IO ()
    failRequest pending = finish pending (Left (failure "ami_disconnected"))

readEvents :: AMIRules -> Socket -> Requests -> (Message -> IO ()) -> IO ()
readEvents rules connection requests observe = loop `finally` closed
  where
    loop :: IO ()
    loop = readMessage rules connection >>= receive
    receive :: Result Message -> IO ()
    receive (Left _) = pure ()
    receive (Right message) = do
      modifyMVar_ requests (update message)
      unless (T.null (field "Event" message)) (observe message)
      loop
    update :: Message -> Map Text Pending -> IO (Map Text Pending)
    update message pending = do
      let (updated, completed) = notify rules message pending
      maybe (pure ()) (uncurry finish) completed
      pure updated
    closed :: IO ()
    closed = disconnect requests >> observe (Message (M.singleton "Event" "AMIConnectionClosed") M.empty)

request :: AMIRules -> Socket -> Requests -> MVar Int -> Text -> [(Text, Text)] -> Maybe Text -> IO (Result [Message])
request rules connection requests sequenceId action fields complete = do
  identifier <- stateMVar sequenceId nextId
  continue (submit rules connection requests identifier complete) (encodeAction rules action identifier fields)
  where
    nextId :: Int -> (Int, Text)
    nextId current = (current + 1, T.pack (show current))

submit :: AMIRules -> Socket -> Requests -> Text -> Maybe Text -> ByteString -> IO (Result [Message])
submit rules connection requests identifier complete payload = do
  result <- newEmptyMVar
  let pending = Pending complete [] result
  updateMVar requests (M.insert identifier pending)
  sendWait result `finally` remove
  where
    uncertain :: Failure
    uncertain = Failure "ami_action_outcome_unknown" True
    sendWait :: MVar (Result [Message]) -> IO (Result [Message])
    sendWait result = do
      let write = ioResult "ami_action_outcome_unknown" (N.sendAll connection payload)
      sent <- within uncertain (amiWriteMicros rules) write
      continue (const (await result)) sent
    await :: MVar (Result [Message]) -> IO (Result [Message])
    await result = within uncertain (amiActionMicros rules) (takeMVar result)
    remove :: IO ()
    remove = updateMVar requests (M.delete identifier)

openSocket :: AMIRules -> IO Socket
openSocket rules = bracketOnError create close establish
  where
    create :: IO Socket
    create = socket AF_INET Stream defaultProtocol
    establish :: Socket -> IO Socket
    establish connection = do
      let address = SockAddrInet (fromIntegral (amiPort rules)) (tupleToHostAddress (127, 0, 0, 1))
      connect connection address
      pure connection

closeAMI :: Socket -> Async () -> IO ()
closeAMI connection reader = do
  cancel reader
  void (ioResult "ami_disconnected" (close connection))

startAMI :: AMIRules -> Socket -> Text -> Text -> (Message -> IO ()) -> IO (Result AMI)
startAMI rules connection username password observe = do
  requests <- newMVar M.empty
  sequenceId <- newMVar 1
  reader <- async (readEvents rules connection requests observe)
  let client = AMI (request rules connection requests sequenceId) (closeAMI connection reader)
  logged <- amiAction client "Login" [("Username", username), ("Secret", password), ("Events", "on")] Nothing `onException` amiClose client
  case logged of
    Left problem -> amiClose client >> pure (Left problem)
    Right _ -> pure (Right client)

connectAMI :: AMIRules -> Text -> Text -> (Message -> IO ()) -> IO (Result AMI)
connectAMI rules username password observe = do
  let deadline = failure "ami_connection_timeout"
      open = ioResult "ami_disconnected" (openSocket rules)
  connected <- within deadline (amiConnectMicros rules) open
  continue (\connection -> verify connection `onException` close connection) connected
  where
    verify :: Socket -> IO (Result AMI)
    verify connection = do
      let bound = amiMessageBytes rules + 1
          readBanner = attempt "ami_disconnected" (readDelimited connection "\n" bound)
          deadline = failure "ami_invalid_banner"
      banner <- within deadline (amiBannerMicros rules) readBanner
      case banner of
        Right bytes | B.isPrefixOf "Asterisk Call Manager/" bytes -> startAMI rules connection username password observe
        _ -> reject connection
    reject :: Socket -> IO (Result AMI)
    reject connection = close connection >> pure (Left (failure "ami_invalid_banner"))

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
