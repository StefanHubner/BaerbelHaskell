module Baerbel.Core.AudioSocket where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( jsonOptions )
import Data.Aeson ( FromJSON(..), genericParseJSON )
import Data.ByteString ( ByteString )
import Data.Text ( Text )
import Data.Word ( Word8 )
import GHC.Generics ( Generic )
import Numeric ( showHex )
import qualified Data.ByteString as B
    ( length, pack, splitAt, unpack )
import qualified Data.Text as T
    ( concat, drop, intercalate, justifyRight, take, pack )

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
      lower = fromIntegral (size `mod` 256)
      header = B.pack [kindByte kind, upper, lower]
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
  case B.splitAt size body of
    (payload, remaining) | B.length payload == size -> Right (Packet kind payload, remaining)
    _ -> Left (failure "audiosocket_incomplete_payload")

uuidText :: ByteString -> Result Text
uuidText bytes
  | B.length bytes /= 16 = Left (failure "audiosocket_invalid_uuid")
  | otherwise = Right (renderUuid bytes)

renderUuid :: ByteString -> Text
renderUuid bytes = T.intercalate "-" groups
  where
    hex :: Word8 -> Text
    hex byte = T.justifyRight 2 '0' (T.pack (showHex byte ""))
    raw :: Text
    raw = T.concat (map hex (B.unpack bytes))
    groups :: [Text]
    groups = map (\(offset, size) -> T.take size (T.drop offset raw))
      [(0, 8), (8, 4), (12, 4), (16, 4), (20, 12)]

readCallId :: Packet -> Result Text
readCallId (Packet CallId bytes) = Right (renderUuid bytes)
readCallId _ = Left (failure "audiosocket_uuid_required")
