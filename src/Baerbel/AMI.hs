{-# LANGUAGE OverloadedStrings #-}
module Baerbel.AMI
  ( AMIRules(..), Message(..), Channel(..), AMI(..), parseMessage, encodeAction
  , readMessage, actionResponse, connectAMI, ownerChannel, incomingChannel
  , field, variable
  ) where

import Baerbel.JSON (jsonOptions)
import Data.Aeson (FromJSON(..), genericParseJSON)
import GHC.Generics (Generic)
import Baerbel.Error (Failure(..), Result, failure, ioResult)
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracketOnError, finally, onException)
import Control.Monad (join, void)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Maybe (isJust, isNothing, fromMaybe)
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
import System.Timeout (timeout)

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
readDelimited connection delimiter limit = go [] B.empty 0
  where
    go :: [ByteString] -> ByteString -> Int -> IO (Result ByteString)
    go chunks ending size
      | ending == delimiter = pure (Right (B.concat (reverse chunks)))
      | size >= limit = pure (Left (failure "ami_event_limit"))
      | otherwise = readNext connection delimiter limit chunks ending size

readNext :: Socket -> ByteString -> Int -> [ByteString] -> ByteString -> Int -> IO (Result ByteString)
readNext connection delimiter limit chunks ending size = do
  byte <- N.recv connection 1
  let next = B.takeEnd (B.length delimiter) (ending <> byte)
  if B.null byte then pure (Left (failure "ami_disconnected")) else continue byte next
  where
    continue :: ByteString -> ByteString -> IO (Result ByteString)
    continue byte next
      | next == delimiter = pure (Right (B.concat (reverse (byte : chunks))))
      | size + 1 >= limit = pure (Left (failure "ami_event_limit"))
      | otherwise = readNext connection delimiter limit (byte : chunks) next (size + 1)

readMessage :: AMIRules -> Socket -> IO (Result Message)
readMessage rules connection = do
  let bound = amiMessageBytes rules + 4
  raw <- ioResult "ami_disconnected" (readDelimited connection "\r\n\r\n" bound)
  pure (parseMessage <$> join raw)

actionResponse :: AMIRules -> Maybe Text -> [Message] -> Message -> Result (Maybe [Message])
actionResponse rules complete previous message
  | field "Response" message == "Error" = Left (failure "ami_action_rejected")
  | Just (field "Event" message) == complete = Right (Just (reverse previous))
  | isJust complete && not (T.null (field "Event" message)) && length previous >= amiEventLimit rules = Left (failure "ami_event_limit")
  | isNothing complete && not (T.null (field "Response" message)) = Right (Just [message])
  | otherwise = Right Nothing

advance :: AMIRules -> Message -> Pending -> IO (Maybe Pending)
advance rules message pending = case actionResponse rules (pendingComplete pending) (pendingEvents pending) message of
  Left problem -> finish pending (Left problem)
  Right (Just result) -> finish pending (Right result)
  Right Nothing -> pure (Just (appendEvent message pending))

appendEvent :: Message -> Pending -> Pending
appendEvent message pending
  | isNothing (pendingComplete pending) || T.null (field "Event" message) = pending
  | otherwise = pending { pendingEvents = message : pendingEvents pending }

finish :: Pending -> Result [Message] -> IO (Maybe Pending)
finish pending result = do
  void (tryPutMVar (pendingResult pending) result)
  pure Nothing

notify :: AMIRules -> Requests -> Message -> IO ()
notify rules requests message = modifyMVar_ requests update
  where
    identifier :: Text
    identifier = field "ActionID" message
    update :: Map Text Pending -> IO (Map Text Pending)
    update pending = maybe (pure pending) (advancePending pending) (M.lookup identifier pending)
    advancePending :: Map Text Pending -> Pending -> IO (Map Text Pending)
    advancePending pending pendingRequest = do
      result <- advance rules message pendingRequest
      pure (maybe (M.delete identifier pending) (\value -> M.insert identifier value pending) result)

disconnect :: Requests -> IO ()
disconnect requests = modifyMVar_ requests clear
  where
    clear :: Map Text Pending -> IO (Map Text Pending)
    clear pending = mapM_ failRequest (M.elems pending) >> pure M.empty
    failRequest :: Pending -> IO ()
    failRequest pending = void (finish pending (Left (failure "ami_disconnected")))

readEvents :: AMIRules -> Socket -> Requests -> (Message -> IO ()) -> IO ()
readEvents rules connection requests observe = loop `finally` closed
  where
    loop :: IO ()
    loop = readMessage rules connection >>= receive
    receive :: Result Message -> IO ()
    receive (Left _) = pure ()
    receive (Right message) = do
      notify rules requests message
      if T.null (field "Event" message) then pure () else observe message
      loop
    closed :: IO ()
    closed = disconnect requests >> observe (Message (M.singleton "Event" "AMIConnectionClosed") M.empty)

request :: AMIRules -> Socket -> Requests -> MVar Int -> Text -> [(Text, Text)] -> Maybe Text -> IO (Result [Message])
request rules connection requests sequenceId action fields complete = do
  identifier <- modifyMVar sequenceId nextId
  case encodeAction rules action identifier fields of
    Left problem -> pure (Left problem)
    Right payload -> submit rules connection requests identifier complete payload
  where
    nextId :: Int -> IO (Int, Text)
    nextId current = pure (current + 1, T.pack (show current))

submit :: AMIRules -> Socket -> Requests -> Text -> Maybe Text -> ByteString -> IO (Result [Message])
submit rules connection requests identifier complete payload = do
  result <- newEmptyMVar
  let pending = Pending complete [] result
  modifyMVar_ requests (pure . M.insert identifier pending)
  sendWait result `finally` remove
  where
    uncertain :: Failure
    uncertain = Failure "ami_action_outcome_unknown" True
    sendWait :: MVar (Result [Message]) -> IO (Result [Message])
    sendWait result = do
      sent <- timeout (amiWriteMicros rules) (ioResult "ami_action_outcome_unknown" (N.sendAll connection payload))
      maybe (pure (Left uncertain)) (either (pure . Left) (const (await result))) sent
    await :: MVar (Result [Message]) -> IO (Result [Message])
    await result = do
      outcome <- timeout (amiActionMicros rules) (takeMVar result)
      pure (fromMaybe (Left uncertain) outcome)
    remove :: IO ()
    remove = modifyMVar_ requests (pure . M.delete identifier)

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
  connected <- timeout (amiConnectMicros rules) (ioResult "ami_disconnected" (openSocket rules))
  case connected of
    Nothing -> pure (Left (failure "ami_connection_timeout"))
    Just (Left problem) -> pure (Left problem)
    Just (Right connection) -> verify connection `onException` close connection
  where
    verify :: Socket -> IO (Result AMI)
    verify connection = do
      let bound = amiMessageBytes rules + 1
      banner <- timeout (amiBannerMicros rules) (ioResult "ami_disconnected" (readDelimited connection "\n" bound))
      if validBanner banner then startAMI rules connection username password observe else reject connection
    reject :: Socket -> IO (Result AMI)
    reject connection = close connection >> pure (Left (failure "ami_invalid_banner"))

validBanner :: Maybe (Result (Result ByteString)) -> Bool
validBanner (Just (Right (Right bytes))) = B.isPrefixOf "Asterisk Call Manager/" bytes
validBanner _ = False

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
