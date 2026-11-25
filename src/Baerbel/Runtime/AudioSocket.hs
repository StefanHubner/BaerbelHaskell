{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.AudioSocket
  ( module Core, boundedWritePacket, readPacket, writePacket, readExact ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Runtime.Effects ( attempt, continue, within )
import Baerbel.Runtime.Error ( ioResult )
import Data.ByteString ( ByteString )
import Network.Socket ( Socket )
import Baerbel.Core.AudioSocket as Core
    ( Packet(..),
      PacketKind(..),
      SocketRules(..),
      frameBytes,
      frameMicros,
      kindByte,
      parseKind,
      validateSize,
      encodePacket,
      parseHeader,
      decodePacket,
      uuidText,
      renderUuid,
      readCallId )
import qualified Data.ByteString as B ( concat, length, uncons )
import qualified Network.Socket.ByteString as N ( recv, sendAll )

readExact :: Socket -> Int -> IO (Result ByteString)
readExact socket size = go size []
  where
    go :: Int -> [ByteString] -> IO (Result ByteString)
    go 0 chunks = pure (Right (B.concat (reverse chunks)))
    go remaining chunks = do
      chunk <- N.recv socket remaining
      let next = remaining - B.length chunk
      case B.uncons chunk of
        Nothing -> pure (Left (failure "audiosocket_incomplete_read"))
        Just _ -> go next (chunk : chunks)

readPacket :: Socket -> IO (Result Packet)
readPacket socket = attempt "audiosocket_connection_failed" operation
  where
    operation :: IO (Result Packet)
    operation = do
      header <- readExact socket 3
      continue (\(kind, size) -> fmap (Packet kind) <$> readExact socket size) (header >>= parseHeader)

writePacket :: Socket -> Packet -> IO (Result ())
writePacket socket packet = continue send (encodePacket packet)
  where send bytes = ioResult "audiosocket_connection_failed" (N.sendAll socket bytes)

boundedWritePacket :: SocketRules -> Socket -> Packet -> IO (Result ())
boundedWritePacket rules socket = within deadline (socketWriteMicros rules) . writePacket socket
  where deadline = failure "audiosocket_write_timeout"
