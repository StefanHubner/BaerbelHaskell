module HTTPSpec (spec) where

import Control.Exception (bracket)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified Data.ByteString as Bytes
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Baerbel.Error (Failure(..), failure)
import Baerbel.HTTP
import qualified Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (Status, status200, status400, status408, status503)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

spec :: Spec
spec = describe "shared HTTP outcome boundary" $ do
  it "does not silently replay a POST after a reused connection closes without a response" $
    bracket openListener Socket.close checkNoReplay
  it "ignores proxy environment variables like the original trust_env=False clients" $
    withProxyEnvironment (compareRequests status200 "not-json" 64 "invalid_response" True)
  it "keeps successful mutation replies uncertain when JSON is malformed" $
    compareRequests status200 "invalid" 64 "invalid_response" True
  it "keeps successful mutation replies uncertain when the bounded body is exceeded" $
    compareRequests status200 "{\"value\":123}" 4 "response_too_large" True
  it "classifies HTTP failures before parsing or reading their error body" $
    compareRequests status503 "not-json-and-too-long" 1 "http_503" True
  it "treats rejected mutations as definite failures" $
    compareRequests status400 "invalid" 64 "http_400" False
  it "treats an upstream timeout as an unknown mutation outcome" $
    compareRequests status408 "invalid" 64 "http_408" True
  it "never marks a failed token refresh as an attempted mutation" $ do
    let token = pure (Left (failure "refresh_scopes_rejected"))
        unexpected _ = expectationFailure "mutation transport was reached" >> pure (Left (failure "unexpected"))
        request = testRequest "POST" "http://127.0.0.1/unused" 64
    authenticated token unexpected request `shouldReturn` Left (failure "refresh_scopes_rejected")

compareRequests :: Status -> Lazy.ByteString -> Int -> Text -> Bool -> Expectation
compareRequests status body limit code uncertain = testWithApplication application check
  where
  application = pure (\_ respond -> respond (Wai.responseLBS status [] body))
  check port = do
    sender <- newSenderWith mutationPolicy
    let url = "http://127.0.0.1:" <> Text.pack (show port)
        readRequest = testRequest "GET" url limit
        writeRequest = testRequest "POST" url limit
    sender readRequest `shouldReturn` Left (Failure code False)
    sender writeRequest `shouldReturn` Left (Failure code uncertain)

testRequest :: ByteString -> Text -> Int -> Request
testRequest method url limit = Request method url [] [] EmptyBody limit 2

mutationPolicy :: Request -> Maybe (Int -> Bool)
mutationPolicy request
  | requestMethod request == "POST" = Just (\status -> status >= 500 || status == 408)
  | otherwise = Nothing

openListener :: IO Socket.Socket
openListener = do
  listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
  Socket.listen listener 2
  pure listener

checkNoReplay :: Socket.Socket -> Expectation
checkNoReplay listener = do
  address <- Socket.getSocketName listener
  case address of
    Socket.SockAddrInet port _ -> run port
    _ -> expectationFailure "unexpected listener address"
  where
    run port = do
      requests <- newMVar []
      withAsync (serveStaleConnection listener requests) (const (check requests port))
    check requests port = do
      sender <- newSenderWith mutationPolicy
      let url = "http://127.0.0.1:" <> Text.pack (show port)
          warm = testRequest "GET" url 64
          mutation = testRequest "POST" url 64
      sender warm `shouldReturn` Right (Data.Aeson.object [])
      sender mutation `shouldReturn` Left (Failure "connection_failed" True)
      reverse <$> readMVar requests `shouldReturn` ["GET", "POST"]

serveStaleConnection :: Socket.Socket -> MVar [ByteString] -> IO ()
serveStaleConnection listener requests = do
  bracket accepted Socket.close first
  bracket accepted Socket.close repeated
  where
    accepted = fst <$> Socket.accept listener
    first connection = do
      readRequestMethod connection requests
      Socket.sendAll connection response
      readRequestMethod connection requests
    repeated connection = readRequestMethod connection requests >> Socket.sendAll connection response
    response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}"

readRequestMethod :: Socket.Socket -> MVar [ByteString] -> IO ()
readRequestMethod connection requests = do
  header <- headers Bytes.empty
  modifyMVar_ requests (pure . (Bytes.takeWhile (/= 32) header :))
  where
    headers bytes
      | "\r\n\r\n" `Bytes.isSuffixOf` bytes = pure bytes
      | otherwise = Socket.recv connection 1 >>= headers . (bytes <>)

withProxyEnvironment :: IO a -> IO a
withProxyEnvironment action = bracket save restore (const (set >> action))
  where
    keys = ["http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "no_proxy", "NO_PROXY"]
    save = traverse (\key -> (,) key <$> lookupEnv key) keys
    set = mapM_ setProxy keys
    setProxy :: String -> IO ()
    setProxy key
      | key `elem` ["no_proxy", "NO_PROXY"] = setEnv key ""
      | otherwise = setEnv key "http://127.0.0.1:1"
    restore = mapM_ (\(key, previous) -> maybe (unsetEnv key) (setEnv key) previous)
