module Baerbel.Core.Speech where

import Baerbel.Core.Error ( Result, failure )
import Data.Bits ( (.|.), shiftL, shiftR )
import Data.Int ( Int16 )
import Data.Word ( Word8 )
import Foreign.C.Types ( CFloat )
import qualified Data.ByteString as BS
    ( ByteString, length, null, pack, splitAt, unpack )

preparePCM :: Bool -> Bool -> BS.ByteString -> BS.ByteString -> Result ([Int16], BS.ByteString)
preparePCM done final pending bytes
  | done || (final && not (BS.null rest)) = Left (failure "invalid_pcm_stream")
  | otherwise = Right (decodeSamples complete, rest)
  where
    combined = pending <> bytes
    size = BS.length combined - BS.length combined `mod` 2
    (complete, rest) = BS.splitAt size combined

clipSample :: CFloat -> Int16
clipSample value = fromIntegral bounded
  where
    rounded = round value :: Int
    bounded = max (-32768) (min 32767 rounded)

decodePCM :: BS.ByteString -> Result [Int16]
decodePCM bytes
  | odd (BS.length bytes) = Left (failure "incomplete_pcm_sample")
  | otherwise = Right (decodeSamples bytes)

decodeSamples :: BS.ByteString -> [Int16]
decodeSamples = pairs . BS.unpack
  where
    pairs :: [Word8] -> [Int16]
    pairs (lo : hi : rest) = sample : pairs rest
      where sample = fromIntegral lo .|. shiftL (fromIntegral hi) 8
    pairs _ = []

encodePCM :: [Int16] -> BS.ByteString
encodePCM samples = BS.pack (concatMap bytes samples)
  where
    bytes value = [fromIntegral value, fromIntegral (shiftR value 8)]

splitFrames :: Int -> BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitFrames size bytes
  | size <= 0 || BS.length bytes < size = ([], bytes)
  | otherwise = let (frame, rest) = BS.splitAt size bytes
                    (frames, pending) = splitFrames size rest
                in (frame : frames, pending)

audiblePCM :: BS.ByteString -> Bool
audiblePCM bytes = either (const False) audible (decodePCM bytes)
  where
    audible [] = False
    audible samples = let squares = map (\sample -> (fromIntegral sample :: Double) ** 2) samples
                          rms = sqrt (sum squares / fromIntegral (length samples))
                      in rms >= 100
