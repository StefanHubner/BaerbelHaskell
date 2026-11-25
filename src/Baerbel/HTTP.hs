module Baerbel.HTTP
  ( Body (..), Request (..), Send, RawSend, StreamSend, Response (..), newSender, newSenderWith, newRawSender, newStreamSender, send, rawSend, streamSend, authenticated, requestJSON ) where

import Baerbel.Effects (continue, mapResult)
import Data.Bifunctor (first)
import Data.Maybe (isJust, fromMaybe)
import Baerbel.Error (Failure (..), Result, failure)
import Baerbel.JSON (decodeBytes)
import Control.Exception (try)
import Data.Aeson (Value (..), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

data Body = EmptyBody | JsonBody Value | FormBody [(ByteString, ByteString)]
  deriving (Eq, Show)

data Request = Request
  { requestMethod :: ByteString
  , requestUrl :: Text
  , requestHeaders :: [(ByteString, ByteString)]
  , requestQuery :: [(ByteString, Maybe ByteString)]
  , requestBody :: Body
  , requestLimit :: Int
  , requestTimeout :: Int
  } deriving (Eq, Show)

data Response = Response
  { responseStatus :: Int, responseHeaders :: [(ByteString, ByteString)]
  , responseBody :: ByteString } deriving (Eq, Show)

type RawSend = Request -> IO (Result Response)
type Send = Request -> IO (Result Value)
type StreamSend = Request -> (ByteString -> IO (Result ())) -> IO (Result ())

requestJSON :: Send -> Request -> (Value -> Result a) -> IO (Result a)
requestJSON sender request parse = mapResult parse (sender request)

newSender :: IO Send
newSender = newSenderWith (const Nothing)

newSenderWith :: (Request -> Maybe (Int -> Bool)) -> IO Send
newSenderWith mutation = sendWith mutation <$> newTransportManager

newRawSender :: IO RawSend
newRawSender = rawSend <$> newTransportManager

newStreamSender :: IO StreamSend
newStreamSender = streamSend <$> newTransportManager

newTransportManager :: IO HTTP.Manager
newTransportManager = HTTP.newManager configured
  where
    direct :: HTTP.ManagerSettings
    direct = HTTP.managerSetProxy HTTP.noProxy tlsManagerSettings
    configured :: HTTP.ManagerSettings
    configured = direct {HTTP.managerRetryableException = const False}

send :: HTTP.Manager -> Send
send = sendWith (const Nothing)

sendWith :: (Request -> Maybe (Int -> Bool)) -> HTTP.Manager -> Send
sendWith mutation manager spec = withResponse uncertain manager spec consumeJSON
  where
  policy = mutation spec
  uncertain = isJust policy
  consumeJSON response = continue readJSON checked
    where
    checked = responseStatusResult (fromMaybe (const False) policy) (statusCode (HTTP.responseStatus response))
    body = readBounded (requestLimit spec) (HTTP.responseBody response)
    readJSON () = first mark <$> mapResult decodeBody body
  mark err = err {failureUncertain = uncertain}

rawSend :: HTTP.Manager -> RawSend
rawSend manager spec = withResponse True manager spec readRaw
  where
  readRaw response = readResponse response <$> readBounded (requestLimit spec) (HTTP.responseBody response)

streamSend :: HTTP.Manager -> StreamSend
streamSend manager spec sink = withResponse False manager spec consumeResponse
  where
  consumeResponse response = continue stream checked
    where
    checked = responseStatusResult (const False) (statusCode (HTTP.responseStatus response))
    stream () = consume (\() chunk -> sink chunk) () (requestLimit spec) (HTTP.responseBody response)

withResponse :: Bool -> HTTP.Manager -> Request -> (HTTP.Response HTTP.BodyReader -> IO (Result a)) -> IO (Result a)
withResponse uncertain manager spec consumeResponse = continue exchange (prepare spec)
  where
  exchange request = do
    outcome <- try (HTTP.withResponse request manager consumeResponse)
    pure (either (transport uncertain) id outcome)

transport :: Bool -> HTTP.HttpException -> Result a
transport _ (HTTP.InvalidUrlException _ _) = Left (failure "connection_failed")
transport uncertain _ = Left (Failure "connection_failed" uncertain)

prepare :: Request -> Result HTTP.Request
prepare spec = maybe (Left (failure "connection_failed")) (Right . configure spec) parsed
  where
  parsed = HTTP.parseRequest (Text.unpack (requestUrl spec)) :: Maybe HTTP.Request

configure :: Request -> HTTP.Request -> HTTP.Request
configure spec parsed = withBody (requestBody spec) configured
  where
  micros = requestTimeout spec * 1000000
  configured = HTTP.setQueryString (requestQuery spec) parsed
    { HTTP.method = requestMethod spec
    , HTTP.requestHeaders = map (first CI.mk) (requestHeaders spec)
    , HTTP.redirectCount = 0
    , HTTP.responseTimeout = HTTP.responseTimeoutMicro micros
    , HTTP.checkResponse = \_ _ -> pure ()
    , HTTP.proxy = Nothing
    }

withBody :: Body -> HTTP.Request -> HTTP.Request
withBody EmptyBody request = request
withBody (FormBody fields) request = HTTP.urlEncodedBody fields request
withBody (JsonBody value) request = request
  { HTTP.requestBody = HTTP.RequestBodyLBS (encode value)
  , HTTP.requestHeaders = ("Content-Type", "application/json") : HTTP.requestHeaders request
  }

readResponse :: HTTP.Response a -> Result ByteString -> Result Response
readResponse response body = Response status headers <$> body
  where
  status = statusCode (HTTP.responseStatus response)
  headers = map (first CI.original) (HTTP.responseHeaders response)

responseStatusResult :: (Int -> Bool) -> Int -> Result ()
responseStatusResult mutation status
  | status >= 200 && status < 300 = Right ()
  | otherwise = Left (Failure ("http_" <> Text.pack (show status)) uncertain)
  where uncertain = mutation status

readBounded :: Int -> HTTP.BodyReader -> IO (Result ByteString)
readBounded limit reader = fmap (BS.concat . reverse <$>) (consume collect [] limit reader)
  where collect chunks chunk = pure (Right (chunk : chunks))

consume :: (a -> ByteString -> IO (Result a)) -> a -> Int -> HTTP.BodyReader -> IO (Result a)
consume step state limit reader = HTTP.brRead reader >>= advance
  where
  advance chunk
    | BS.null chunk = pure (Right state)
    | BS.length chunk > limit = pure (Left (failure "response_too_large"))
    | otherwise = advanceState chunk
  advanceState chunk = do
    next <- step state chunk
    let remaining = limit - BS.length chunk
    continue (\value -> consume step value remaining reader) next

decodeBody :: ByteString -> Result Value
decodeBody bytes = decodeBytes "invalid_response" bytes >>= jsonObject

jsonObject :: Value -> Result Value
jsonObject value@(Object _) = Right value
jsonObject _ = Left (failure "invalid_response")

authenticated :: IO (Result Text) -> Send -> Send
authenticated token base request = do
  credential <- token
  continue (base . authorize) credential
  where
  authorize value = request
    { requestHeaders = ("Authorization", "Bearer " <> Text.encodeUtf8 value) : requestHeaders request }
