module LiveCloseSpec (spec) where

import Baerbel.Core.Config (Global, User(..), resolveSettings)
import Baerbel.Core.Error (failure)
import Baerbel.Core.Live (LiveConfig(..), closedStatus)
import Baerbel.Core.Protocol (closeCommand)
import Baerbel.Core.Session (announcementConfig)
import Baerbel.Runtime.Live (closeSession)
import Baerbel.Runtime.Config (readGlobal)
import Control.Concurrent.Async (AsyncCancelled, cancel, link, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, fromException)
import Control.Monad (void)
import Data.Aeson (Value, decodeStrict', encode, object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import Network.Socket (Family(AF_UNIX), SocketType(Stream), close, defaultProtocol, socketPair)
import System.Timeout (timeout)
import Test.Hspec
import qualified Data.ByteString as Bytes
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

spec :: Spec
spec = describe "Live provider finalization" $ do
  it "uses configured close budgets capped by the provider timeout" $ withGlobal $ \global -> do
    let settings = resolveSettings global (User (Map.singleton "OPENAI_TIMEOUT_SECONDS" "2"))
        budgets cfg = (liveCloseMicros cfg, liveStartupCloseMicros cfg)
    fmap budgets (announcementConfig settings "") `shouldBe` Right (2000000, 100000)
  it "sends close and waits through unrelated events for final usage" $ withConfig $ \cfg -> do
    received <- newEmptyMVar
    let server conn = readCommand conn >>= putMVar received >> sendAcknowledgement conn
        client conn = do
          closeSession cfg True False conn `shouldReturn` Right (Just acknowledgement)
          takeMVar received `shouldReturn` Just closeCommand
    withPeer server client
    closedStatus acknowledgement `shouldBe` (True, Just usage)
  it "times out without inventing provider finalization" $ withConfig $ \cfg -> do
    blocked <- newEmptyMVar
    let server conn = void (readCommand conn) >> takeMVar blocked
        client conn = closeSession cfg True False conn `shouldReturn` Left (failure "live_close_timeout")
    withPeer server client
  it "only sends close after failed startup without waiting for an acknowledgement" $ withConfig $ \cfg -> do
    received <- newEmptyMVar
    let server conn = readCommand conn >>= putMVar received
        client conn = do
          closeSession cfg False False conn `shouldReturn` Right Nothing
          takeMVar received `shouldReturn` Just closeCommand
    withPeer server client
  it "does not send another close after an already finalized session" $ withConfig $ \cfg -> do
    received <- newEmptyMVar
    let server conn = timeout 20000 (readCommand conn) >>= putMVar received
        client conn = do
          closeSession cfg True True conn `shouldReturn` Right Nothing
          takeMVar received `shouldReturn` Nothing
    withPeer server client
  it "returns operational peer closure as a value" $ withConfig $ \cfg -> do
    let server conn = void (readCommand conn) >> WS.sendClose conn ("finished" :: Text)
        client conn = closeSession cfg True False conn `shouldReturn` Left (failure "websocket_transport_failed")
    withPeer server client
  it "preserves cancellation while waiting for final usage" $ withConfig cancellation

acknowledgement :: Value
acknowledgement = object ["type" .= ("session.closed" :: Text), "usage" .= usage]

usage :: Value
usage = object ["total_tokens" .= (7 :: Int)]

sendAcknowledgement :: WS.Connection -> IO ()
sendAcknowledgement conn = do
  let unrelated = object ["type" .= ("session.output_transcript.delta" :: Text), "delta" .= ("finished" :: Text)]
  WS.sendTextData conn (encode unrelated)
  WS.sendTextData conn (encode acknowledgement)

cancellation :: LiveConfig -> Expectation
cancellation cfg = do
  received <- newEmptyMVar
  blocked <- newEmptyMVar
  let server conn = void (readCommand conn) >> putMVar received () >> takeMVar blocked
      client conn = withAsync (closeSession cfg {liveCloseMicros = 2000000} True False conn) (cancelAfter received)
  withPeer server client
  where
    cancelAfter received worker = do
      void (takeMVar received)
      cancel worker
      result <- waitCatch worker
      case result of
        Left issue -> isJust (fromException issue :: Maybe AsyncCancelled) `shouldBe` True
        Right _ -> expectationFailure "Provider-close cancellation was swallowed"

withGlobal :: (Global -> Expectation) -> Expectation
withGlobal action = do
  loaded <- readGlobal "config/global.yaml"
  either (expectationFailure . show) action loaded

withConfig :: (LiveConfig -> Expectation) -> Expectation
withConfig action = withGlobal $ \global -> do
  let settings = resolveSettings global (User Map.empty)
      configured = announcementConfig settings ""
      bounded cfg = cfg {liveCloseMicros = 50000, liveStartupCloseMicros = 20000}
  either (expectationFailure . show) (action . bounded) configured

withPeer :: (WS.Connection -> IO ()) -> (WS.Connection -> Expectation) -> Expectation
withPeer server client = bracket create release run
  where
    create = socketPair AF_UNIX Stream defaultProtocol
    release (first, second) = close first >> close second
    run (first, second) = withAsync (serve first) $ \worker -> do
      link worker
      WS.runClientWithSocket second "localhost" "/" WS.defaultConnectionOptions [] client
    serve socket = WS.makePendingConnection socket WS.defaultConnectionOptions >>= WS.acceptRequest >>= server

readCommand :: WS.Connection -> IO (Maybe Value)
readCommand conn = decodeStrict' <$> (WS.receiveData conn :: IO Bytes.ByteString)
