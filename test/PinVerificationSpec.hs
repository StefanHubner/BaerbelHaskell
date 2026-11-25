module PinVerificationSpec (spec) where

import Baerbel.Core.Error (failure)
import Baerbel.Runtime.Pin (PinConfig(..), PinGate(..), newPinGate)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.Aeson (Value(..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, expectationFailure, shouldReturn)
import qualified Data.ByteString as Bytes

spec :: Spec
spec = describe "verified PIN handoff" $ do
  it "publishes verification before releasing the pending tool, once per call" ordering
  it "keeps the gate closed when the verified-state notification fails" failedNotification
  it "cancels a blocked verification notification without releasing the tool" cancelledNotification

withGate :: (PinGate -> IO ()) -> IO ()
withGate action = do
  opened <- newPinGate config
  either (expectationFailure . show) (\gate -> finally (action gate) (closePin gate)) opened
  where
    config = PinConfig encoded ["gmail_"] 60000000 4 8 (String "prompt") (String "incorrect") (String "verified")

encoded :: Text
encoded = "pbkdf2_sha256:600000:abababababababababababababababab:fc1b8ef76285a4b6ff59805b7fcaee34702d5054376d15a96e02105daaef3ee1"

submit :: PinGate -> IO ()
submit gate = mapM_ (feedDigit gate . Bytes.singleton) (Bytes.unpack "5678#")

ordering :: IO ()
ordering = withGate $ \gate -> do
  prompted <- newEmptyMVar
  verifying <- newEmptyMVar
  release <- newEmptyMVar
  notices <- newIORef []
  let notify event = modifyIORef' notices (<> [event]) >> deliver event
      deliver (String "prompt") = putMVar prompted () >> pure (Right ())
      deliver _ = putMVar verifying () >> takeMVar release >> pure (Right ())
      guard = prepareTool gate notify "gmail_search"
  withAsync guard $ \pending -> do
    takeMVar prompted >> submit gate >> takeMVar verifying
    timeout 20000 (wait pending) `shouldReturn` Nothing
    collecting gate `shouldReturn` True
    putMVar release ()
    wait pending `shouldReturn` Right ()
    prepareTool gate notify "gmail_read" `shouldReturn` Right ()
    readIORef notices `shouldReturn` [String "prompt", String "verified"]

failedNotification :: IO ()
failedNotification = withGate $ \gate -> do
  prompted <- newEmptyMVar
  let notify (String "prompt") = putMVar prompted () >> pure (Right ())
      notify _ = pure (Left (failure "synthetic_notification_failed"))
  withAsync (prepareTool gate notify "gmail_search") $ \pending -> do
    takeMVar prompted >> submit gate
    timeout 5000000 (wait pending) `shouldReturn` Just (Left (failure "pin_unavailable"))
    collecting gate `shouldReturn` False
    prepareTool gate notify "gmail_read" `shouldReturn` Left (failure "pin_unavailable")

cancelledNotification :: IO ()
cancelledNotification = withGate $ \gate -> do
  prompted <- newEmptyMVar
  verifying <- newEmptyMVar
  block <- newEmptyMVar
  let notify (String "prompt") = putMVar prompted () >> pure (Right ())
      notify _ = putMVar verifying () >> takeMVar block
  withAsync (prepareTool gate notify "gmail_search") $ \pending -> do
    takeMVar prompted >> submit gate >> takeMVar verifying
    timeout 500000 (closePin gate) `shouldReturn` Just ()
    wait pending `shouldReturn` Left (failure "pin_unavailable")
    collecting gate `shouldReturn` False
