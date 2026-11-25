
module Baerbel.Runtime.HTTP
  ( module Core, Send, RawSend, newSender, newSenderWith, newRawSender, send, rawSend, authenticated, requestJSON ) where

import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Runtime.Effects ( continue, mapResult )
import Control.Exception ( try )
import Data.Aeson ( Value(..), encode )
import Data.Bifunctor ( first )
import Data.ByteString ( ByteString )
import Data.Maybe ( isJust, fromMaybe )
import Data.Text ( Text )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Network.HTTP.Types.Status ( statusCode )
import Baerbel.Core.HTTP as Core
    ( Response(..),
      Request(..),
      Body(..),
      responseStatusResult,
      decodeBody,
      jsonObject )
import qualified Data.ByteString as BS ( concat, length, null )
import qualified Data.CaseInsensitive as CI ( CI(original), mk )
import qualified Network.HTTP.Client as HTTP
    ( managerSetProxy,
      responseTimeoutMicro,
      brRead,
      withResponse,
      newManager,
      noProxy,
      parseRequest,
      setQueryString,
      urlEncodedBody,
      BodyReader,
      HttpException(InvalidUrlException),
      Manager,
      ManagerSettings(managerRetryableException),
      Request(proxy, requestBody, method, requestHeaders, redirectCount,
              responseTimeout, checkResponse),
      RequestBody(RequestBodyLBS),
      Response(responseHeaders, responseBody, responseStatus) )
import qualified Data.Text as Text ( unpack )
import qualified Data.Text.Encoding as Text ( encodeUtf8 )

type RawSend = Request -> IO (Result Response)
type Send = Request -> IO (Result Value)

requestJSON :: Send -> Request -> (Value -> Result a) -> IO (Result a)
requestJSON sender request parse = mapResult parse (sender request)

newSender :: IO Send
newSender = newSenderWith (const Nothing)

newSenderWith :: (Request -> Maybe (Int -> Bool)) -> IO Send
newSenderWith mutation = sendWith mutation <$> newTransportManager

newRawSender :: IO RawSend
newRawSender = rawSend <$> newTransportManager

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

authenticated :: IO (Result Text) -> (Request -> IO (Result a)) -> Request -> IO (Result a)
authenticated token base request = do
  credential <- token
  continue (base . authorize) credential
  where
  authorize value = request
    { requestHeaders = ("Authorization", "Bearer " <> Text.encodeUtf8 value) : requestHeaders request }
