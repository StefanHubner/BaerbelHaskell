module MetricsSpec (spec) where

import Baerbel.Core.AudioMetrics
import Baerbel.Runtime.AudioSocket (SocketRules(..), readPacket, Packet(..), PacketKind(Audio))
import Baerbel.Core.Error (failure)
import Baerbel.Runtime.Config (loadSettings, rule)
import Baerbel.Runtime.Live (Media(..))
import Baerbel.Runtime.ManagedMedia (newRegistry, withManaged, ManagedCall(..))
import Baerbel.Runtime.Ringback (withRingback)
import Baerbel.Runtime.Server (socketMedia)
import Control.Exception (bracket)
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import Network.Socket (Socket, socketPair, Family(AF_UNIX), SocketType(Stream), defaultProtocol, close, setSocketOption, SocketOption(SendBuffer))
import Test.Hspec

spec :: Spec
spec = do
  describe "Delivered telephone audio metrics" $ do
    it "ignores paused/ringback suppression and deferred preview frames" $ do
      let initial = emptyMetrics 1000000000
      recordDelivery 1020000000 Suppressed True initial `shouldBe` (initial, [])
      recordDelivery 1020000000 Deferred True initial `shouldBe` (initial, [])
    it "distinguishes first delivered silence from first audible speech once" $ do
      let initial = emptyMetrics 1000000000
          (silent, first) = recordDelivery 1012345678 Delivered False initial
          (speech, second) = recordDelivery 1045678901 Delivered True silent
          (later, repeated) = recordDelivery 1050000000 Delivered True speech
      first `shouldBe` [AudioStarted 12.35]
      second `shouldBe` [SpeechStarted 45.68]
      repeated `shouldBe` []
      metricsOutputFrames later `shouldBe` 3
      metricsFirstAudioMS later `shouldBe` Just 12.35
      metricsFirstSpeechMS later `shouldBe` Just 45.68
    it "reports the original frame-clock and queue high-water units" $ do
      let initial = emptyMetrics 0
          input = foldr ($) initial (replicate 3 recordInputFrame)
          queued = recordQueueDepth 1 (recordQueueDepth 5 input)
          (played, _) = recordDelivery 20000000 Delivered True queued
          expected = object ["type" .= ("phone.call_summary" :: Text), "input_audio_ms" .= (60 :: Int),
            "output_audio_ms" .= (20 :: Int), "max_output_queue_ms" .= (100 :: Int),
            "first_audio_ms" .= (20 :: Int), "first_non_silent_audio_ms" .= (20 :: Int)]
      summaryValue 20000 played `shouldBe` expected
    it "marks a completed AudioSocket write as delivered and a paused write as suppressed" $ withPair $ \client peer -> do
      registry <- newRegistry (SocketRules 5000000 5000000) 30000000
      let pcm = BS.replicate 320 0
      result <- withManaged registry "call" client $ \managed -> do
        sendAudio (managedMedia managed) pcm `shouldReturn` Right Delivered
        readPacket peer `shouldReturn` Right (Packet Audio pcm)
        pauseManaged managed
        sendAudio (managedMedia managed) pcm `shouldReturn` Right Suppressed
        pure (Right ())
      result `shouldBe` Right ()
    it "bounds a blocked managed telephone write by the configured transport deadline" $ withPair $ \client _ -> do
      setSocketOption client SendBuffer 1024
      let limits = SocketRules 5000000 20000
          pcm = BS.replicate 64000 1
      registry <- newRegistry limits 30000000
      result <- withManaged registry "blocked" client $ \managed -> do
        sendAudio (managedMedia managed) pcm `shouldReturn` Left (failure "audiosocket_write_timeout")
        pure (Right ())
      result `shouldBe` Right ()
    it "suppresses model silence while startup ringback is still active" $ withPair $ \client _ -> do
      configured <- loadSettings "config/global.json" "config/user.example.json"
      case configured >>= \settings -> rule settings "startup_ringback" "playback" of
        Left problem -> expectationFailure (show problem)
        Right rules -> do
          let transport = socketMedia (SocketRules 5000000 5000000) client (const (pure ())) (const (pure ()))
              silence = BS.replicate 320 0
          withRingback rules transport (`sendAudio` silence) `shouldReturn` Right Suppressed

withPair :: (Socket -> Socket -> IO ()) -> IO ()
withPair action = bracket create finish (uncurry action)
  where
    create :: IO (Socket, Socket)
    create = socketPair AF_UNIX Stream defaultProtocol
    finish :: (Socket, Socket) -> IO ()
    finish (client, peer) = close client >> close peer
