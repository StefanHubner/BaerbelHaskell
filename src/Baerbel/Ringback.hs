module Baerbel.Ringback (RingRules(..), ringFrame, withRingback) where

import Baerbel.AudioMetrics (AudioDelivery(..))
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
ringFrame rules index = if audible then tone else BS.replicate (ringFrameSamples rules * 2) 0
  where
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
  either (pure . Left) (const advance) result
  where
    advance :: IO (Result a)
    advance = do
      active <- readTVarIO (ringActive ring)
      if not active then atomically retry else next
    next :: IO (Result a)
    next = waitTone ring >> runTone rules media ring epoch (index + 1)

writeTone :: RingRules -> Media -> Ring -> Int -> IO (Result ())
writeTone rules media ring index = do
  active <- readTVarIO (ringActive ring)
  if not active then pure (Right ()) else do
    result <- sendAudio media (ringFrame rules index)
    now <- getMonotonicTimeNSec
    let deadline = now + fromIntegral (ringFrameMicros rules) * 1000
    atomically (writeTVar (ringUntil ring) deadline)
    pure (void result)

writeSpeech :: Media -> Ring -> BS.ByteString -> IO (Result AudioDelivery)
writeSpeech media ring frame = do
  active <- readTVarIO (ringActive ring)
  if active && not (audiblePCM frame) then pure (Right Suppressed) else do
    atomically (writeTVar (ringActive ring) False)
    withMVar (ringLock ring) (const deliver)
  where
    deliver = waitTone ring >> sendAudio media frame

waitTone :: Ring -> IO ()
waitTone ring = do
  deadline <- readTVarIO (ringUntil ring)
  now <- getMonotonicTimeNSec
  let micros = if deadline > now then fromIntegral ((deadline - now) `div` 1000) else 0
  threadDelay micros
