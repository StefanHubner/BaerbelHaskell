{-# LANGUAGE ScopedTypeVariables #-}
module Baerbel.WebSocket (withSecureSocket) where

import Baerbel.Error (Result, failure)
import Control.Exception (IOException, Handler(..), catches)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.Connection (HostNotResolved, HostCannotConnect)
import Network.TLS (TLSException)
import qualified Network.WebSockets as WS
import qualified Wuss

-- Handshake errors may carry request headers; expose only controlled error codes.
withSecureSocket :: forall a. Int -> Text -> String -> Int -> String -> (WS.Connection -> IO (Result a)) -> IO (Result a)
withSecureSocket limit key host port path action = connect `catches` handlers
  where
    headers = [("Authorization", "Bearer " <> Text.encodeUtf8 key)]
    service = fromIntegral port
    limitBytes = fromIntegral limit
    options = WS.defaultConnectionOptions {WS.connectionFramePayloadSizeLimit = WS.SizeLimit limitBytes,
      WS.connectionMessageDataSizeLimit = WS.SizeLimit limitBytes}
    connect = Wuss.runSecureClientWith host service path options headers action
    handlers = [Handler (redacted :: IOException -> IO (Result a)),
      Handler (redacted :: WS.ConnectionException -> IO (Result a)),
      Handler (redacted :: WS.HandshakeException -> IO (Result a)),
      Handler (redacted :: TLSException -> IO (Result a)),
      Handler (redacted :: HostNotResolved -> IO (Result a)),
      Handler (redacted :: HostCannotConnect -> IO (Result a))]
    redacted :: e -> IO (Result a)
    redacted _ = pure (Left (failure "websocket_transport_failed"))
