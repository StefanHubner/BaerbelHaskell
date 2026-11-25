module Baerbel.Cache (memoize, onceSuccess, insertBounded) where

import qualified Baerbel.Effects as Effects
import Baerbel.Concurrent (stateMVar)
import Baerbel.Error (Result)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

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

insertBounded :: Ord key => Int -> key -> value -> (Map key value, [key]) -> (Map key value, [key])
insertBounded capacity key value (entries, order) = (retained, kept)
  where
    added
      | Map.member key entries = order
      | otherwise = order <> [key]
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
