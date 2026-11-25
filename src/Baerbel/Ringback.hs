module Baerbel.Ringback (RingRules(..), ringFrame, withRingback) where

import Baerbel.AudioMetrics (AudioDelivery(..))
import qualified Baerbel.Effects as Effects
import Baerbel.Error (Result)
import Baerbel.JSON (jsonOptions)
import Baerbel.Live (Media(..))
import Baerbel.Speech (audiblePCM, encodePCM)
import Control.Monad (when, void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Data.Aeson (FromJSON(..), genericParseJSON)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)

data RingRules = RingRules
  { ringFrequencies :: [Double], ringAmplitude :: Double, ringFrameSamples :: Int
  , ringSampleRate :: Int, ringFrameMicros :: Int, ringCycleFrames :: Int
  , ringToneRanges :: [(Int, Int)]
  } deriving (Eq, Show, Generic)
instance FromJSON RingRules where parseJSON = genericParseJSON (jsonOptions 4)

data Ring = Ring { ringActive :: TVar Bool, ringUntil :: TVar Word64, ringLock :: MVar () }

ringFrame :: RingRules -> Int -> BS.ByteString
ringFrame rules index
  | audible = tone
  | otherwise = BS.replicate silenceBytes 0
  where
    silenceBytes = ringFrameSamples rules * 2
    phase = index `mod` ringCycleFrames rules
    audible = any (\(start, end) -> phase >= start && phase < end) (ringToneRanges rules)
    sample number = round (ringAmplitude rules * sum (map (wave number) (ringFrequencies rules)))
    wave number frequency = sin (2 * pi * frequency * fromIntegral number / fromIntegral (ringSampleRate rules))
    tone = encodePCM (map sample [0 .. ringFrameSamples rules - 1])

withRingback :: RingRules -> Media -> (Media -> IO (Result a)) -> IO (Result a)
withRingback rules media action = do
  active <- newTVarIO True
  deadline <- newTVarIO 0
  lock <- newMVar ()
  epoch <- mediaEpoch media
  let ring = Ring active deadline lock
      wrapped = media { sendAudio = writeSpeech media ring }
      tones = runTone rules media ring epoch 0
  result <- race tones (action wrapped)
  pure (either id id result)

runTone :: RingRules -> Media -> Ring -> Int -> Int -> IO (Result a)
runTone rules media ring epoch index = do
  current <- mediaEpoch media
  paused <- mediaPaused media
  when (paused || current /= epoch) (atomically (writeTVar (ringActive ring) False))
  result <- withMVar (ringLock ring) (const (writeTone rules media ring index))
  Effects.continue (const advance) result
  where
    advance :: IO (Result a)
    advance = do
      active <- readTVarIO (ringActive ring)
      let continue | active = next
                   | otherwise = atomically retry
      continue
    next :: IO (Result a)
    next = waitTone ring >> runTone rules media ring epoch (index + 1)

writeTone :: RingRules -> Media -> Ring -> Int -> IO (Result ())
writeTone rules media ring index = do
  active <- readTVarIO (ringActive ring)
  let play | active = deliver
           | otherwise = pure (Right ())
  play
  where
    deliver = do
      let frame = ringFrame rules index
      result <- sendAudio media frame
      now <- getMonotonicTimeNSec
      let deadline = now + fromIntegral (ringFrameMicros rules) * 1000
      atomically (writeTVar (ringUntil ring) deadline)
      pure (void result)

writeSpeech :: Media -> Ring -> BS.ByteString -> IO (Result AudioDelivery)
writeSpeech media ring frame = do
  active <- readTVarIO (ringActive ring)
  let play | active && not (audiblePCM frame) = pure (Right Suppressed)
           | otherwise = do
               atomically (writeTVar (ringActive ring) False)
               withMVar (ringLock ring) (const deliver)
  play
  where
    deliver = waitTone ring >> sendAudio media frame

waitTone :: Ring -> IO ()
waitTone ring = do
  deadline <- readTVarIO (ringUntil ring)
  now <- getMonotonicTimeNSec
  let micros | deadline > now = fromIntegral ((deadline - now) `div` 1000)
             | otherwise = 0
  threadDelay micros
