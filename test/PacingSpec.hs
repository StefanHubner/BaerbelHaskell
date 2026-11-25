module PacingSpec (spec) where

import Baerbel.Core.Concurrent (frameDelay, frameSchedule)
import Data.Word (Word64)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "continuous telephone audio pacing" $ do
  it "does not move the next deadline for ordinary scheduler lateness" $ do
    frameSchedule 20000 20000000 21000000 `shouldBe` (0, 40000000)
  it "rebases a real stall instead of bursting overdue frames" $ do
    let first = frameSchedule 20000 20000000 150000000
        second = frameSchedule 20000 (snd first) 150000000
    first `shouldBe` (0, 170000000)
    second `shouldBe` (20000, 190000000)
  it "rounds a fractional wait up without advancing the audio clock" $ do
    frameSchedule 20000 20000000 1001 `shouldBe` (19999, 40000000)
  it "prevents the observed five-second input overflow on a four-minute call" $ do
    let wakes = replicate 12000 1000000
        (elapsed, backlog) = simulate (frameSchedule 20000) wakes
        (_, formerBacklog) = simulate relativeSchedule wakes
    backlog `shouldSatisfy` (< 2)
    formerBacklog `shouldSatisfy` (> 250)
    elapsed `shouldSatisfy` (<= 240000000000)
  it "absorbs varied work and wake delays without accumulating queue latency" $ do
    let wakes = take 12000 (cycle [0, 1000000, 3000000, 12000000])
        (_, backlog) = simulate (frameSchedule 20000) wakes
    backlog `shouldSatisfy` (< 2)

-- A telephone contributes one frame each 20 ms, independently of the sender.
-- Model 300 us work and timer overshoot; no wall-clock sleeps or provider mocks.
simulate :: (Word64 -> Word64 -> (Int, Word64)) -> [Word64] -> (Word64, Int)
simulate schedule wakes = (elapsed, maximumQueued)
  where
    (_, elapsed, maximumQueued) = foldl' advance (0, 0, 0) (zip [0 :: Word64 ..] wakes)
    advance :: (Word64, Word64, Int) -> (Word64, Word64) -> (Word64, Word64, Int)
    advance (deadline, now, peak) (index, overshoot) = (next, sent + 300000, max peak queued)
      where
        (delay, next) = schedule deadline now
        jitter | delay > 0 = overshoot
               | otherwise = 0
        sent = now + fromIntegral delay * 1000 + jitter
        queued = max 0 (fromIntegral (sent `div` 20000000) - fromIntegral index)

-- Negative control: the removed loop restarted its frame budget after each wake.
relativeSchedule :: Word64 -> Word64 -> (Int, Word64)
relativeSchedule _ _ = (frameDelay 20000 0 300000, 0)
