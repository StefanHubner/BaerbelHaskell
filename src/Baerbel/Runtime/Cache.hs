module Baerbel.Runtime.Cache (memoize, onceSuccess, insertBounded) where

import Baerbel.Core.Cache
    ( Cache, lookupEntry, insertEntry, insertBounded )
import Baerbel.Runtime.Concurrent ( stateMVar )
import Baerbel.Runtime.Error ( Result )
import Control.Concurrent.MVar ( MVar, modifyMVar, newMVar )
import qualified Baerbel.Runtime.Effects as Effects
    ( continue, monotonicSeconds )
import qualified Data.Map.Strict as Map ( empty )

memoize :: Ord key => Int -> Double -> (query -> key) -> (query -> IO (Result a)) -> IO (query -> IO (Result a))
memoize capacity ttl keyOf operation = do
  state <- newMVar Map.empty
  pure (cachedRequest state capacity ttl keyOf operation)

cachedRequest :: Ord key => MVar (Cache key a) -> Int -> Double -> (query -> key) -> (query -> IO (Result a)) -> query -> IO (Result a)
cachedRequest state capacity ttl keyOf operation query = do
  now <- Effects.monotonicSeconds
  let key = keyOf query
  cached <- stateMVar state (lookupEntry now ttl key)
  maybe (loadEntry state capacity key (operation query)) (pure . Right) cached

loadEntry :: Ord key => MVar (Cache key a) -> Int -> key -> IO (Result a) -> IO (Result a)
loadEntry state capacity key operation = do
  result <- operation
  now <- Effects.monotonicSeconds
  let store value = stateMVar state (\cache -> (insertEntry capacity now key value cache, Right value))
  Effects.continue store result

onceSuccess :: IO (Result a) -> IO (IO (Result a))
onceSuccess operation = do
  state <- newMVar Nothing
  pure (modifyMVar state resolve)
  where
    resolve (Just value) = pure (Just value, Right value)
    resolve Nothing = fmap retain operation
    retain result = (either (const Nothing) Just result, result)
