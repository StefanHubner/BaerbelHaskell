module Baerbel.Core.Ringback where

import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.Speech (encodePCM)
import Data.Aeson (FromJSON(..), genericParseJSON)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)

data RingRules = RingRules
  { ringFrequencies :: [Double], ringAmplitude :: Double, ringFrameSamples :: Int
  , ringSampleRate :: Int, ringFrameMicros :: Int, ringCycleFrames :: Int
  , ringToneRanges :: [(Int, Int)]
  } deriving (Eq, Show, Generic)
instance FromJSON RingRules where parseJSON = genericParseJSON (jsonOptions 4)

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
