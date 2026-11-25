module Baerbel.Core.Cache where

import Data.List ( sortOn )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
    ( delete, lookup, member, toList, fromList, insert )

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

insertBounded :: Ord key => Int -> key -> value -> (Map key value, [key]) -> (Map key value, [key])
insertBounded capacity key value (entries, order) = (retained, kept)
  where
    added
      | Map.member key entries = order
      | otherwise = order <> [key]
    count = max 0 (length added - capacity)
    (removed, kept) = splitAt count added
    retained = foldl' (flip Map.delete) (Map.insert key value entries) removed
