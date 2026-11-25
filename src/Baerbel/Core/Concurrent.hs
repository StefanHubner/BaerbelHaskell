module Baerbel.Core.Concurrent where

import Data.Word ( Word64 )

hasElapsed :: Int -> Word64 -> Word64 -> Bool
hasElapsed micros now before = now >= before && now - before >= duration
  where
    duration = fromIntegral micros * 1000

frameDelay :: Int -> Word64 -> Word64 -> Int
frameDelay duration started now = max 0 (duration - elapsed)
  where elapsed = fromIntegral ((now - started) `div` 1000)

-- Carry the intended cadence forward; only a genuine stall rebases the clock.
frameSchedule :: Int -> Word64 -> Word64 -> (Int, Word64)
frameSchedule micros deadline now = (delay, due + period)
  where
    period = fromIntegral micros * 1000
    due | now > deadline && now - deadline > period = now
        | otherwise = deadline
    delay | due > now = fromIntegral ((due - now + 999) `div` 1000)
          | otherwise = 0
