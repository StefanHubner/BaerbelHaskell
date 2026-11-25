module Baerbel.Core.Concurrent where

import Data.Word ( Word64 )

hasElapsed :: Int -> Word64 -> Word64 -> Bool
hasElapsed micros now before = now >= before && now - before >= duration
  where
    duration = fromIntegral micros * 1000

frameDelay :: Int -> Word64 -> Word64 -> Int
frameDelay duration started now = max 0 (duration - elapsed)
  where elapsed = fromIntegral ((now - started) `div` 1000)
