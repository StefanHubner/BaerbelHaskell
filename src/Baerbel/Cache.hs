module Baerbel.Cache (memoize, onceSuccess, insertBounded) where

import Baerbel.Error (Result)
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Clock (getMonotonicTimeNSec)

data Entry a = Entry {createdAt :: Double, accessedAt :: Double, entryValue :: a}
type Cache key a = Map key (Entry a)

lookupEntry :: Ord key => Double -> Double -> key -> Cache key a -> (Cache key a, Maybe a)
lookupEntry now ttl key cache = case Map.lookup key cache of
  Just item | now - createdAt item < ttl -> (Map.insert key (item {accessedAt = now}) cache, Just (entryValue item))
  _ -> (cache, Nothing)

insertEntry :: Ord key => Int -> Double -> key -> a -> Cache key a -> Cache key a
insertEntry capacity now key value cache = Map.fromList retained
  where
    updated = Map.insert key (Entry now now value) cache
    ordered = sortOn (accessedAt . snd) (Map.toList updated)
    retained = drop (max 0 (length ordered - capacity)) ordered

seconds :: IO Double
seconds = fmap ((/ 1000000000) . fromIntegral) getMonotonicTimeNSec

memoize :: Ord key => Int -> Double -> (query -> key) -> (query -> IO (Result a)) -> IO (query -> IO (Result a))
memoize capacity ttl keyOf operation = do
  state <- newMVar Map.empty
  let lookupValue now key = modifyMVar state (pure . lookupEntry now ttl key)
      store now key value = modifyMVar state (\cache -> pure (insertEntry capacity now key value cache, Right value))
      request query = do
        now <- seconds
        let key = keyOf query
        cached <- lookupValue now key
        maybe (load key query) (pure . Right) cached
      load key query = do
        result <- operation query
        now <- seconds
        either (pure . Left) (store now key) result
  pure request

insertBounded :: Ord key => Int -> key -> value -> (Map key value, [key]) -> (Map key value, [key])
insertBounded capacity key value (entries, order) = (retained, kept)
  where
    added = if Map.member key entries then order else order <> [key]
    count = max 0 (length added - capacity)
    (removed, kept) = splitAt count added
    retained = foldl' (flip Map.delete) (Map.insert key value entries) removed


onceSuccess :: IO (Result a) -> IO (IO (Result a))
onceSuccess operation = do
  state <- newMVar Nothing
  pure (modifyMVar state resolve)
  where
    resolve (Just value) = pure (Just value, Right value)
    resolve Nothing = fmap retain operation
    retain result = (either (const Nothing) Just result, result)
