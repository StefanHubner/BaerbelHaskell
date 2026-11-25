module Baerbel.Core.Recording where

import Baerbel.Core.Error ( Result, failure )
import Data.ByteString ( ByteString )
import Data.Char ( isAlphaNum, isAscii )
import Data.Foldable ( toList )
import Data.Sequence ( Seq, ViewL(..), ViewR(..), (|>), (<|) )
import Data.Text ( Text )
import qualified Data.ByteString as B
    ( drop, length, pack, take, unpack )
import qualified Data.Sequence as S
    ( drop, empty, length, null, singleton, spanl, viewl, viewr )
import qualified Data.Text as T ( all, length, uncons )

data Anchor = Anchor { anchorTime :: Double, anchorSamples :: Int }
  deriving (Eq, Show)
data ArchiveState = ArchiveState
  { archiveAnchors :: Seq Anchor, archiveSamples :: Int, archiveFinalized :: Bool }
  deriving (Eq, Show)
data Segment = Segment { segmentFirst :: Int, segmentLast :: Int, segmentTime :: Double }
  deriving (Eq, Show)
data Timeline = Timeline
  { pendingFrames :: Seq (Int, Double), sentFrames :: Seq Segment, totalSamples :: Int }
  deriving (Eq, Show)

emptyTimeline :: Timeline
emptyTimeline = Timeline S.empty S.empty 0

addFrame :: Double -> Timeline -> Timeline
addFrame timestamp timeline = timeline { pendingFrames = pendingFrames timeline |> (480, timestamp) }

resetTimeline :: Timeline -> Timeline
resetTimeline timeline = timeline { pendingFrames = S.empty }

sentSamples :: Int -> Timeline -> Result Timeline
sentSamples 0 timeline = Right timeline
sentSamples samples timeline = case S.viewl (pendingFrames timeline) of
  EmptyL -> Left (failure "recording_timeline_missing_frame")
  (count, timestamp) :< remaining -> consumeFrame samples timeline count timestamp remaining

consumeFrame :: Int -> Timeline -> Int -> Double -> Seq (Int, Double) -> Result Timeline
consumeFrame samples timeline count timestamp remaining = sentSamples left updated
  where
    used = min samples count
    end = totalSamples timeline + used
    segment = Segment (totalSamples timeline) end timestamp
    retained = sentFrames timeline |> segment
    bounded = S.drop (max 0 (S.length retained - 30000)) retained
    nextFrames
      | used == count = remaining
      | otherwise = (count - used, timestamp + fromIntegral used / 24000) <| remaining
    updated = Timeline nextFrames bounded end
    left = samples - used

transcriptBounds :: Double -> Double -> Timeline -> Maybe (Double, Double)
transcriptBounds start end timeline
  | start < 0 || end < start = Nothing
  | otherwise = (,) <$> firstMatch starts <*> lastMatch ends
  where
    first :: Double
    first = start * 24
    lastSample :: Double
    lastSample = end * 24
    segments :: [Segment]
    segments = toList (sentFrames timeline)
    starts :: [Double]
    starts = [segmentTime segment + (first - fromIntegral (segmentFirst segment)) / 24000
      | segment <- segments, fromIntegral (segmentFirst segment) <= first, first < fromIntegral (segmentLast segment)]
    ends :: [Double]
    ends = [segmentTime segment + (lastSample - fromIntegral (segmentFirst segment)) / 24000
      | segment <- segments, fromIntegral (segmentFirst segment) < lastSample, lastSample <= fromIntegral (segmentLast segment)]

firstMatch :: [a] -> Maybe a
firstMatch [] = Nothing
firstMatch (value : _) = Just value

lastMatch :: [a] -> Maybe a
lastMatch = firstMatch . reverse

validateWav :: ByteString -> Result ()
validateWav header
  | B.length header == 44 && valid = Right ()
  | otherwise = Left (failure "recording_unsupported_format")
  where
    format :: ByteString
    format = B.pack [16,0,0,0,1,0,1,0,64,31,0,0,128,62,0,0,2,0,16,0]
    valid :: Bool
    valid = B.take 4 header == "RIFF" && B.take 8 (B.drop 8 header) == "WAVEfmt "
      && B.take 20 (B.drop 16 header) == format && B.take 4 (B.drop 36 header) == "data"

littleWord :: ByteString -> Int
littleWord bytes = sum (zipWith (\shift byte -> fromIntegral byte * 256 ^ shift) ([0..] :: [Int]) (B.unpack bytes))

sampleState :: Double -> Int -> Int -> ArchiveState -> ArchiveState
sampleState modified size declared previous = ArchiveState anchors (max samples (archiveSamples previous)) finalized
  where
    samples :: Int
    samples = max 0 (size - 44) `div` 2
    initial :: Seq Anchor
    initial
      | S.null (archiveAnchors previous) = S.singleton (Anchor (modified - fromIntegral samples / 8000) 0)
      | otherwise = archiveAnchors previous
    anchors :: Seq Anchor
    anchors
      | samples > archiveSamples previous = initial |> Anchor modified samples
      | otherwise = archiveAnchors previous
    finalized :: Bool
    finalized = declared > 0 && declared == samples * 2

sampleBounds :: Seq Anchor -> Double -> Result (Double, Double)
sampleBounds anchors timestamp = case (S.viewl anchors, S.viewr anchors) of
  (first :< _, _ :> final) -> Right (projectSamples timestamp first final before after)
  _ -> Left (failure "recording_no_samples")
  where
    (before, after) = S.spanl (\anchor -> anchorTime anchor <= timestamp) anchors

projectSamples :: Double -> Anchor -> Anchor -> Seq Anchor -> Seq Anchor -> (Double, Double)
projectSamples timestamp first final before after = sampleRange timestamp lower upper low high
  where
    (lower, low) = case S.viewr before of
      EmptyR -> (first, 0)
      _ :> value -> (value, fromIntegral (anchorSamples value))
    upper = case S.viewl after of
      EmptyL -> final
      value :< _ -> value
    high = fromIntegral (anchorSamples upper)

sampleRange :: Double -> Anchor -> Anchor -> Double -> Double -> (Double, Double)
sampleRange timestamp left right low high = (min a b, max a b)
  where
    a = project left
    b = project right
    project anchor = max low (min high position)
      where position = fromIntegral (anchorSamples anchor) + (timestamp - anchorTime anchor) * 8000

data Interval = Interval Double Double

clipInterval :: Double -> Double -> Result Interval
clipInterval start end
  | not (finite start && finite end) || duration <= 0 || duration > 600 = Left (failure "recording_invalid_interval")
  | otherwise = Right (Interval start end)
  where
    duration = end - start
    finite value = not (isNaN value || isInfinite value)

clipBounds :: ArchiveState -> Double -> Double -> Result (Int, Int)
clipBounds state start end = clipInterval start end >>= intervalBounds state

intervalBounds :: ArchiveState -> Interval -> Result (Int, Int)
intervalBounds state (Interval start end) = do
  (first, _) <- sampleBounds (archiveAnchors state) (start - 0.2)
  (_, lastSample) <- sampleBounds (archiveAnchors state) (end + 0.2)
  excerptBounds first lastSample

excerptBounds :: Double -> Double -> Result (Int, Int)
excerptBounds first lastSample
  | count > 0 && count <= 9671936 = Right (offset, count)
  | otherwise = Left (failure "recording_excerpt_exceeds_bounds")
  where
    begin = floor first
    count = (ceiling lastSample - begin) * 2
    offset = 44 + begin * 2

validIdentity :: Text -> Bool
validIdentity identity = T.length identity <= 100 && case T.uncons identity of
  Nothing -> False
  Just (first, rest) -> asciiName first && T.all allowed rest
  where
    asciiName :: Char -> Bool
    asciiName char = isAscii char && isAlphaNum char
    allowed :: Char -> Bool
    allowed char = asciiName char || char `elem` ("_.-" :: String)

readExcerpt :: Int -> ByteString -> Result ByteString
readExcerpt size bytes
  | B.length bytes == size = Right bytes
  | otherwise = Left (failure "recording_incomplete_excerpt")
