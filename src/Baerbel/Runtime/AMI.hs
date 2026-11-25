{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.AMI
  ( module Core, AMI(..), connectAMI, startAMI ) where

import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Runtime.Concurrent ( stateMVar, updateMVar )
import Baerbel.Runtime.Effects ( attempt, continue, within )
import Baerbel.Runtime.Error ( ioResult )
import Control.Concurrent
    ( MVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar )
import Control.Concurrent.Async ( Async, async, cancel )
import Control.Exception ( bracketOnError, finally, onException )
import Control.Monad ( void, unless )
import Data.ByteString ( ByteString )
import Data.Map.Strict ( Map )
import Data.Maybe ( isNothing )
import Data.Text ( Text )
import Network.Socket
    ( Socket,
      Family(AF_INET),
      SocketType(Stream),
      SockAddr(SockAddrInet),
      close,
      connect,
      defaultProtocol,
      socket,
      tupleToHostAddress )
import Baerbel.Core.AMI as Core
    ( Channel(..),
      Message(..),
      AMIRules(..),
      field,
      variable,
      parseMessage,
      addLine,
      addVariable,
      validField,
      encodeAction,
      actionResponse,
      ownerChannel,
      ownerMatch,
      incomingChannel,
      incomingMatch,
      incomingName,
      selectChannel )
import qualified Data.ByteString as B
    ( concat, isPrefixOf, length, takeEnd, uncons, empty )
import qualified Data.Map.Strict as M
    ( elems, singleton, delete, insert, empty, lookup )
import qualified Network.Socket.ByteString as N ( recv, sendAll )
import qualified Data.Text as T ( null, pack )

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
