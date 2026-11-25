module EffectsSpec (spec) where

import Baerbel.Runtime.Effects (attempt, continue, runAll, withResult, within)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Control.Concurrent.Async (withAsync, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Either (isLeft)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Test.Hspec

spec :: Spec
spec = describe "shared execution boundaries" $ do
  it "does not execute a continuation after failure" $ do
    calls <- newIORef (0 :: Int)
    let issue = Failure "unconfirmed_delivery" True
        operation () = modifyIORef' calls (+ 1) >> pure (Right ())
    continue operation (Left issue) `shouldReturn` Left issue
    readIORef calls `shouldReturn` 0
  it "stops a sequence before later effects and retains uncertainty" $ do
    calls <- newIORef []
    let issue = Failure "unconfirmed_delivery" True
        operation n = modifyIORef' calls (<> [n]) >> pure (outcome n)
        outcome :: Int -> Result ()
        outcome 2 = Left issue
        outcome _ = Right ()
    runAll operation [1, 2, 3] `shouldReturn` Left issue
    readIORef calls `shouldReturn` [1, 2]
  it "does not use or release a failed acquisition" $ do
    calls <- newIORef ([] :: [String])
    let acquire = pure (Left (failure "unavailable")) :: IO (Result ())
        release () = modifyIORef' calls (<> ["release"])
        use () = modifyIORef' calls (<> ["use"]) >> pure (Right ())
    withResult acquire release use `shouldReturn` Left (failure "unavailable")
    readIORef calls `shouldReturn` []
  it "releases once when an acquired operation returns a failure" $ do
    calls <- newIORef ([] :: [String])
    let acquire = pure (Right ())
        release () = modifyIORef' calls (<> ["release"])
        use () = modifyIORef' calls (<> ["use"]) >> pure (Left (failure "failed")) :: IO (Result ())
    withResult acquire release use `shouldReturn` Left (failure "failed")
    readIORef calls `shouldReturn` ["use", "release"]
  it "preserves cancellation through the exception boundary and releases once" cancellation
  it "retains the configured uncertainty on timeout" $ do
    blocked <- newEmptyMVar
    let issue = Failure "delivery_timeout" True
        action = attempt "io_failed" (takeMVar blocked) :: IO (Result ())
    within issue 1000 action `shouldReturn` Left issue

cancellation :: Expectation
cancellation = do
  started <- newEmptyMVar
  blocked <- newEmptyMVar
  released <- newIORef (0 :: Int)
  let acquire = pure (Right ())
      release () = modifyIORef' released (+ 1)
      use () = putMVar started () >> takeMVar blocked :: IO (Result ())
      operation = attempt "io_failed" (withResult acquire release use)
  withAsync operation $ \worker -> do
    takeMVar started
    cancel worker
    waitCatch worker >>= (`shouldSatisfy` isLeft)
  readIORef released `shouldReturn` 1
