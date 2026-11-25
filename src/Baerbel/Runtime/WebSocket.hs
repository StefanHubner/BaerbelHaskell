{-# LANGUAGE ScopedTypeVariables #-}
module Baerbel.Runtime.WebSocket (withSecureSocket, socketResult) where

import Baerbel.Core.Error ( Result, failure )
import Control.Exception ( IOException, Handler(..), catches )
import Data.Text ( Text )
import Network.Connection ( HostNotResolved, HostCannotConnect )
import Network.TLS ( TLSException )
import qualified Wuss ( runSecureClientWith )
import qualified Data.Text.Encoding as Text ( encodeUtf8 )
import qualified Network.WebSockets as WS
    ( defaultConnectionOptions,
      Connection,
      ConnectionOptions(connectionMessageDataSizeLimit,
                        connectionFramePayloadSizeLimit),
      SizeLimit(SizeLimit),
      HandshakeException,
      ConnectionException )

withSecureSocket :: forall a. Int -> Text -> String -> Int -> String -> (WS.Connection -> IO (Result a)) -> IO (Result a)
withSecureSocket limit key host port path action = socketResult connect
  where
    headers = [("Authorization", "Bearer " <> Text.encodeUtf8 key)]
    service = fromIntegral port
    limitBytes = fromIntegral limit
    options = WS.defaultConnectionOptions {WS.connectionFramePayloadSizeLimit = WS.SizeLimit limitBytes,
      WS.connectionMessageDataSizeLimit = WS.SizeLimit limitBytes}
    connect = Wuss.runSecureClientWith host service path options headers action

socketResult :: forall a. IO (Result a) -> IO (Result a)
socketResult action = action `catches` handlers
  where
    handlers = [Handler (redacted :: IOException -> IO (Result a)),
      Handler (redacted :: WS.ConnectionException -> IO (Result a)),
      Handler (redacted :: WS.HandshakeException -> IO (Result a)),
      Handler (redacted :: TLSException -> IO (Result a)),
      Handler (redacted :: HostNotResolved -> IO (Result a)),
      Handler (redacted :: HostCannotConnect -> IO (Result a))]
    redacted :: e -> IO (Result a)
    redacted _ = pure (Left (failure "websocket_transport_failed"))
