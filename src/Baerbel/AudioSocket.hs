{-# LANGUAGE OverloadedStrings #-}
module Baerbel.AudioSocket
  ( SocketRules(..), boundedWritePacket, PacketKind(..), Packet(..), encodePacket, decodePacket, readPacket
  , writePacket, readCallId, uuidText, frameBytes, frameMicros
  ) where

import Baerbel.JSON (jsonOptions)
import Data.Aeson (FromJSON(..), genericParseJSON)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Baerbel.Error (Result, failure, ioResult)
import Control.Monad (join)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as N
import Numeric (showHex)

data SocketRules = SocketRules
  { socketIdentityMicros :: Int, socketWriteMicros :: Int } deriving (Eq, Show, Generic)
instance FromJSON SocketRules where parseJSON = genericParseJSON (jsonOptions 6)

data PacketKind = Hangup | CallId | Dtmf | Audio | ProtocolError
  deriving (Eq, Show)

data Packet = Packet { packetKind :: PacketKind, packetPayload :: ByteString }
  deriving (Eq, Show)

frameBytes :: Int
frameBytes = 320

frameMicros :: Int
frameMicros = 20000

kindByte :: PacketKind -> Word8
kindByte Hangup = 0
kindByte CallId = 1
kindByte Dtmf = 3
kindByte Audio = 16
kindByte ProtocolError = 255

parseKind :: Word8 -> Result PacketKind
parseKind 0 = Right Hangup
parseKind 1 = Right CallId
parseKind 3 = Right Dtmf
parseKind 16 = Right Audio
parseKind 255 = Right ProtocolError
parseKind _ = Left (failure "audiosocket_unsupported_type")

validateSize :: PacketKind -> Int -> Result ()
validateSize kind size
  | size < 0 || size > 65535 = Left (failure "audiosocket_payload_too_large")
  | kind == Hangup && size /= 0 = Left (failure "audiosocket_invalid_hangup")
  | kind == CallId && size /= 16 = Left (failure "audiosocket_invalid_uuid")
  | kind == Dtmf && size /= 1 = Left (failure "audiosocket_invalid_dtmf")
  | kind == Audio && (size == 0 || odd size) = Left (failure "audiosocket_invalid_audio")
  | otherwise = Right ()

encodePacket :: Packet -> Result ByteString
encodePacket (Packet kind payload) = do
  let size = B.length payload
  validateSize kind size
  let upper = fromIntegral (size `div` 256)
  let lower = fromIntegral (size `mod` 256)
  let header = B.pack [kindByte kind, upper, lower]
  pure (header <> payload)

parseHeader :: ByteString -> Result (PacketKind, Int)
parseHeader bytes = case B.unpack bytes of
  [tag, upper, lower] -> do
    kind <- parseKind tag
    let size = fromIntegral upper * 256 + fromIntegral lower
    validateSize kind size
    pure (kind, size)
  _ -> Left (failure "audiosocket_incomplete_header")

decodePacket :: ByteString -> Result (Packet, ByteString)
decodePacket bytes = do
  let (header, body) = B.splitAt 3 bytes
  (kind, size) <- parseHeader header
  let (payload, remaining) = B.splitAt size body
  if B.length payload == size
    then Right (Packet kind payload, remaining)
    else Left (failure "audiosocket_incomplete_payload")

readExact :: Socket -> Int -> IO (Result ByteString)
readExact socket size = go size []
  where
    go :: Int -> [ByteString] -> IO (Result ByteString)
    go 0 chunks = pure (Right (B.concat (reverse chunks)))
    go remaining chunks = do
      chunk <- N.recv socket remaining
      let next = remaining - B.length chunk
      if B.null chunk then pure (Left (failure "audiosocket_incomplete_read")) else go next (chunk : chunks)

readBody :: Socket -> Result ByteString -> IO (Result Packet)
readBody _ (Left problem) = pure (Left problem)
readBody socket (Right header) = case parseHeader header of
  Left problem -> pure (Left problem)
  Right (kind, size) -> do
    payload <- readExact socket size
    pure (Packet kind <$> payload)

readPacket :: Socket -> IO (Result Packet)
readPacket socket = do
  result <- ioResult "audiosocket_connection_failed" operation
  pure (join result)
  where
    operation :: IO (Result Packet)
    operation = readExact socket 3 >>= readBody socket

writePacket :: Socket -> Packet -> IO (Result ())
writePacket socket packet = case encodePacket packet of
  Left problem -> pure (Left problem)
  Right bytes -> ioResult "audiosocket_connection_failed" (N.sendAll socket bytes)

boundedWritePacket :: SocketRules -> Socket -> Packet -> IO (Result ())
boundedWritePacket rules socket packet = do
  result <- timeout (socketWriteMicros rules) (writePacket socket packet)
  pure (fromMaybe (Left (failure "audiosocket_write_timeout")) result)

uuidText :: ByteString -> Result Text
uuidText bytes
  | B.length bytes /= 16 = Left (failure "audiosocket_invalid_uuid")
  | otherwise = Right (T.intercalate "-" groups)
  where
    hex :: Word8 -> Text
    hex byte = T.justifyRight 2 '0' (T.pack (showHex byte ""))
    raw :: Text
    raw = T.concat (map hex (B.unpack bytes))
    groups :: [Text]
    groups = map (\(offset, size) -> T.take size (T.drop offset raw))
      [(0, 8), (8, 4), (12, 4), (16, 4), (20, 12)]

readCallId :: Socket -> IO (Result Text)
readCallId socket = do
  packet <- readPacket socket
  pure (packet >>= extract)
  where
    extract :: Packet -> Result Text
    extract (Packet CallId bytes) = uuidText bytes
    extract _ = Left (failure "audiosocket_uuid_required")
