{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Server
  ( ServerConfig(..), CallHandler, serve, socketMedia, runListener
  ) where

import Baerbel.Concurrent (raceWorkers)
import Baerbel.AudioSocket (SocketRules(..), boundedWritePacket, Packet(..), PacketKind(..), readCallId, readPacket)
import Baerbel.Error (Failure, Result, failure, ioResult)
import Baerbel.AudioMetrics (AudioDelivery(Delivered))
import Baerbel.Live (Media(..))
import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, mask)
import Control.Monad (join)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Data.Text (Text)
import Network.Socket
import System.Timeout (timeout)

type CallHandler = Text -> Socket -> IO (Result ())

data ServerConfig = ServerConfig
  { serverHost :: HostName, ownerPort :: ServiceName
  , incomingPort :: Maybe ServiceName, homePort :: Maybe ServiceName
  , connectionLimit :: Int, ownerHandler :: CallHandler
  , incomingHandler :: CallHandler, homeHandler :: Socket -> IO (Result ())
  , serverSocketRules :: SocketRules, reportFailure :: Failure -> IO ()
  }

serve :: ServerConfig -> IO (Result ())
serve cfg = withSocketsDo (raceWorkers listeners)
  where
    owner = runListener cfg (ownerPort cfg) (identified (serverSocketRules cfg) (ownerHandler cfg))
    incoming = maybe [] (\port -> [runListener cfg port (identified (serverSocketRules cfg) (incomingHandler cfg))]) (incomingPort cfg)
    home = maybe [] (\port -> [runListener cfg port (homeHandler cfg)]) (homePort cfg)
    listeners = owner : incoming ++ home

identified :: SocketRules -> CallHandler -> Socket -> IO (Result ())
identified rules handler client = do
  result <- timeout (socketIdentityMicros rules) (readPacket client)
  let identity = maybe (Left (failure "audiosocket_identity_timeout")) (>>= readCallId) result
  either (pure . Left) (`handler` client) identity

runListener :: ServerConfig -> ServiceName -> (Socket -> IO (Result ())) -> IO (Result ())
runListener cfg port handler = do
  result <- ioResult "listener_failed" (openListener cfg port handler)
  pure (join result)

openListener :: ServerConfig -> ServiceName -> (Socket -> IO (Result ())) -> IO (Result ())
openListener cfg port handler = do
  let hints = defaultHints { addrSocketType = Stream }
  addresses <- getAddrInfo (Just hints) (Just (serverHost cfg)) (Just port)
  case addresses of
    [] -> pure (Left (failure "listener_address_unavailable"))
    address : _ -> bracket (openSocket address) close (runAccept cfg address handler)

runAccept :: ServerConfig -> AddrInfo -> (Socket -> IO (Result ())) -> Socket -> IO (Result ())
runAccept cfg address handler listener = do
  setSocketOption listener ReuseAddr 1
  bind listener (addrAddress address)
  listen listener (connectionLimit cfg)
  children <- newTVarIO Set.empty
  finally (acceptLoop cfg handler listener children) (stopChildren children)

stopChildren :: TVar (Set.Set ThreadId) -> IO ()
stopChildren children = do
  active <- readTVarIO children
  mapM_ killThread active
  atomically (readTVar children >>= check . Set.null)

acceptLoop :: ServerConfig -> (Socket -> IO (Result ())) -> Socket -> TVar (Set.Set ThreadId) -> IO (Result ())
acceptLoop cfg handler listener children = mask $ \restore -> do
  (client, _) <- restore (accept listener)
  count <- Set.size <$> readTVarIO children
  admit count client
  restore (acceptLoop cfg handler listener children)
  where
    admit count client
      | count >= connectionLimit cfg = close client
      | otherwise = startChild cfg handler client children

startChild :: ServerConfig -> (Socket -> IO (Result ())) -> Socket -> TVar (Set.Set ThreadId) -> IO ()
startChild cfg handler client children = do
  gate <- newEmptyMVar
  tid <- forkIOWithUnmask $ \unmask -> do
    ident <- takeMVar gate
    let remove = atomically (modifyTVar' children (Set.delete ident))
    finally (finally (unmask (handleClient cfg handler client)) (close client)) remove
  atomically (modifyTVar' children (Set.insert tid))
  putMVar gate tid

handleClient :: ServerConfig -> (Socket -> IO (Result ())) -> Socket -> IO ()
handleClient cfg handler client = do
  result <- ioResult "call_transport_failed" (handler client)
  either (reportFailure cfg) (const (pure ())) (join result)

socketMedia :: SocketRules -> Socket -> (ByteString -> IO ()) -> (Value -> IO ()) -> Media
socketMedia rules client keypad observer = Media
  { receiveAudio = readAudio client keypad
  , sendAudio = fmap (fmap (const Delivered)) . boundedWritePacket rules client . Packet Audio
  , finishCall = boundedWritePacket rules client (Packet Hangup mempty)
  , observeEvent = observer
  , muteInput = pure False
  , beforeTool = \_ _ -> pure (Right ())
  , installControls = const (pure ())
  , observeInputAudio = const (pure ())
  , afterTool = \_ _ -> pure ()
  , mediaPaused = pure False
  , mediaEpoch = pure 0, progressSuppressed = pure False
  }

readAudio :: Socket -> (ByteString -> IO ()) -> IO (Result (Maybe ByteString))
readAudio client keypad = do
  packet <- readPacket client
  case packet of
    Left err -> pure (Left err)
    Right (Packet Hangup _) -> pure (Right Nothing)
    Right (Packet Audio bytes) -> pure (Right (Just bytes))
    Right (Packet Dtmf digit) -> keypad digit >> readAudio client keypad
    Right _ -> pure (Left (failure "unexpected_audiosocket_packet"))
