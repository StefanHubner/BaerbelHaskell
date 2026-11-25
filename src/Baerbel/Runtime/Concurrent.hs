module Baerbel.Runtime.Concurrent (raceWorkers, awaitReady, hasElapsed, stateMVar, updateMVar, paceFrame) where

import Baerbel.Core.Concurrent ( hasElapsed, frameDelay )
import Baerbel.Runtime.Error ( Result, failure )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Concurrent.MVar ( MVar, modifyMVar, modifyMVar_ )
import Control.Concurrent.STM
    ( atomically, retry, check, TVar, readTVar )
import Data.Text ( Text )
import Data.Word ( Word64 )
import GHC.Clock ( getMonotonicTimeNSec )
import System.Timeout ( timeout )

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


stateMVar :: MVar s -> (s -> (s, a)) -> IO a
stateMVar cell step = modifyMVar cell (pure . step)

updateMVar :: MVar s -> (s -> s) -> IO ()
updateMVar cell step = modifyMVar_ cell (pure . step)

paceFrame :: Int -> Word64 -> IO ()
paceFrame duration started = do
  now <- getMonotonicTimeNSec
  let remaining = frameDelay duration started now
  threadDelay remaining
