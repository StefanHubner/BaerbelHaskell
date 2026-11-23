module Baerbel.Concurrent (raceWorkers, awaitReady, hasElapsed) where

import Baerbel.Error (Result, failure)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Data.Text (Text)
import Data.Word (Word64)
import System.Timeout (timeout)

raceWorkers :: [IO (Result ())] -> IO (Result ())
raceWorkers [] = pure (Right ())
raceWorkers [worker] = worker
raceWorkers (worker : workers) = do
  result <- race worker (raceWorkers workers)
  pure (either id id result)

awaitReady :: Text -> Int -> TVar Bool -> IO (Result ())
awaitReady code duration started = do
  result <- timeout duration (atomically (readTVar started >>= check))
  case result of
    Nothing -> pure (Left (failure code))
    Just () -> atomically retry


hasElapsed :: Int -> Word64 -> Word64 -> Bool
hasElapsed micros now before = now >= before && now - before >= duration
  where
    duration = fromIntegral micros * 1000
