module Baerbel.Worker (withWorker) where

import Baerbel.Error (Result)
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.STM
import Control.Exception (finally)

-- Accepted requests belong to this scope; cancelling a waiter does not remove
-- its queued operation. Closing appends the stop marker after all accepted work.
data Work request response = Work request (TMVar (Result response)) | Stop

withWorker :: (request -> IO (Result response)) -> ((request -> IO (Result response)) -> IO (Result a)) -> IO (Result a)
withWorker operation action = do
  queue <- newTQueueIO
  withAsync (runWorker operation queue) $ \worker ->
    action (submit queue) `finally` (atomically (writeTQueue queue Stop) >> wait worker)

submit :: TQueue (Work request response) -> request -> IO (Result response)
submit queue request = do
  result <- atomically $ do
    response <- newEmptyTMVar
    writeTQueue queue (Work request response)
    pure response
  atomically (readTMVar result)

runWorker :: (request -> IO (Result response)) -> TQueue (Work request response) -> IO ()
runWorker operation queue = atomically (readTQueue queue) >>= process
  where
    process Stop = pure ()
    process (Work request result) = do
      response <- operation request
      atomically (putTMVar result response)
      runWorker operation queue
