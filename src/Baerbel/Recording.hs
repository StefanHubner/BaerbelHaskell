{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Recording
  ( Anchor(..), ArchiveState(..), Archive, sampleBounds, clipBounds, validateWav
  , sampleState, openRecording, closeRecording, readClip
  , Timeline, emptyTimeline, addFrame, resetTimeline, sentSamples, transcriptBounds
  ) where

import Baerbel.Error (Result, failure, ioResult)
import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar, threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (onException)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>), (<|))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word64)
import Foreign.C.Error (Errno, eNOENT, getErrno)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CLong(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>))
import System.Posix.Files (FileStatus, fileSize, getFdStatus, modificationTimeHiRes)
import System.Posix.IO (closeFd)
import System.Posix.Types (CSsize(..), Fd(..))

foreign import ccall safe "open" nativeOpen :: CString -> CInt -> IO CInt
foreign import ccall safe "pread" nativeRead :: CInt -> Ptr Word8 -> CSize -> CLong -> IO CSsize

data Anchor = Anchor { anchorTime :: Double, anchorSamples :: Int }
  deriving (Eq, Show)
data ArchiveState = ArchiveState
  { archiveAnchors :: Seq Anchor, archiveSamples :: Int, archiveFinalized :: Bool }
  deriving (Eq, Show)
data Archive = Archive
  { archiveFd :: Fd, archiveCurrent :: MVar (Result ArchiveState), archivePoller :: Async () }

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
  (count, timestamp) :< remaining -> do
    let used = min samples count
    let end = totalSamples timeline + used
    let segment = Segment (totalSamples timeline) end timestamp
    let retained = sentFrames timeline |> segment
    let bounded = S.drop (max 0 (S.length retained - 30000)) retained
    let nextFrames = if used == count then remaining else (count - used, timestamp + fromIntegral used / 24000) <| remaining
    let updated = Timeline nextFrames bounded end
    sentSamples (samples - used) updated

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
    initial = if S.null (archiveAnchors previous) then S.singleton (Anchor (modified - fromIntegral samples / 8000) 0) else archiveAnchors previous
    anchors :: Seq Anchor
    anchors = if samples > archiveSamples previous then initial |> Anchor modified samples else archiveAnchors previous
    finalized :: Bool
    finalized = declared > 0 && declared == samples * 2

sampleBounds :: Seq Anchor -> Double -> Result (Double, Double)
sampleBounds anchors timestamp = case (S.viewr before, S.viewl after, S.viewl anchors, S.viewr anchors) of
  (_, _, EmptyL, _) -> Left (failure "recording_no_samples")
  (left, right, first :< _, _ :> final) -> Right (project left right first final)
  _ -> Left (failure "recording_no_samples")
  where
    before :: Seq Anchor
    after :: Seq Anchor
    (before, after) = S.spanl (\anchor -> anchorTime anchor <= timestamp) anchors
    project :: ViewR Anchor -> ViewL Anchor -> Anchor -> Anchor -> (Double, Double)
    project left right first final = bounds lower upper low high
      where
        lower :: Anchor
        lower = case left of { EmptyR -> first; _ :> value -> value }
        upper :: Anchor
        upper = case right of { EmptyL -> final; value :< _ -> value }
        low :: Double
        low = if S.null before then 0 else fromIntegral (anchorSamples lower)
        high :: Double
        high = fromIntegral (anchorSamples upper)
    bounds :: Anchor -> Anchor -> Double -> Double -> (Double, Double)
    bounds left right low high = (min a b, max a b)
      where
        a :: Double
        a = max low (min high (fromIntegral (anchorSamples left) + (timestamp - anchorTime left) * 8000))
        b :: Double
        b = max low (min high (fromIntegral (anchorSamples right) + (timestamp - anchorTime right) * 8000))

clipBounds :: ArchiveState -> Double -> Double -> Result (Int, Int)
clipBounds state start end
  | not (finite start && finite end) || spanSeconds <= 0 || spanSeconds > 600 = Left (failure "recording_invalid_interval")
  | otherwise = do
      (first, _) <- sampleBounds (archiveAnchors state) (start - 0.2)
      (_, lastSample) <- sampleBounds (archiveAnchors state) (end + 0.2)
      let begin = floor first
      let count = (ceiling lastSample - begin) * 2
      if count > 0 && count <= 9671936 then Right (44 + begin * 2, count) else Left (failure "recording_excerpt_exceeds_bounds")
  where
    spanSeconds :: Double
    spanSeconds = end - start
    finite :: Double -> Bool
    finite value = not (isNaN value || isInfinite value)

pread :: Fd -> Int -> Int -> IO (Result ByteString)
pread (Fd descriptor) offset count = allocaBytes count readBuffer
  where
    readBuffer :: Ptr Word8 -> IO (Result ByteString)
    readBuffer buffer = do
      size <- nativeRead descriptor buffer (fromIntegral count) (fromIntegral offset)
      if size < 0 then pure (Left (failure "recording_read_failed")) else Right <$> B.packCStringLen (castPtr buffer, fromIntegral size)

observe :: Fd -> ArchiveState -> IO (Result ArchiveState)
observe descriptor previous = do
  stat <- ioResult "recording_read_failed" (getFdStatus descriptor)
  declared <- pread descriptor 40 4
  pure ((,) <$> stat <*> declared >>= uncurry update)
  where
    update :: FileStatus -> ByteString -> Result ArchiveState
    update stat bytes
      | B.length bytes /= 4 = Left (failure "recording_read_failed")
      | otherwise = Right (sampleState (realToFrac (modificationTimeHiRes stat)) (fromIntegral (fileSize stat)) (littleWord bytes) previous)

poll :: Fd -> MVar (Result ArchiveState) -> IO ()
poll descriptor current = do
  threadDelay 100000
  outcome <- modifyMVar current sample
  case outcome of
    Left _ -> pure ()
    Right _ -> poll descriptor current
  where
    sample :: Result ArchiveState -> IO (Result ArchiveState, Result ArchiveState)
    sample (Left problem) = pure (Left problem, Left problem)
    sample (Right previous) = do
      result <- observe descriptor previous
      pure (result, result)

validIdentity :: Text -> Bool
validIdentity identity = T.length identity <= 100 && case T.uncons identity of
  Nothing -> False
  Just (first, rest) -> asciiName first && T.all allowed rest
  where
    asciiName :: Char -> Bool
    asciiName char = isAscii char && isAlphaNum char
    allowed :: Char -> Bool
    allowed char = asciiName char || char `elem` ("_.-" :: String)

openDescriptor :: FilePath -> IO (Result (Maybe Fd))
openDescriptor path = withCString path open
  where
    open :: CString -> IO (Result (Maybe Fd))
    open name = do
      descriptor <- nativeOpen name 131072
      opened descriptor <$> getErrno
    opened :: CInt -> Errno -> Result (Maybe Fd)
    opened descriptor errno
      | descriptor >= 0 = Right (Just (Fd descriptor))
      | errno == eNOENT = Right Nothing
      | otherwise = Left (failure "recording_open_failed")

openRecording :: FilePath -> Text -> IO (Result Archive)
openRecording directory identity
  | not (validIdentity identity) = pure (Left (failure "recording_invalid_identity"))
  | otherwise = do
      now <- getMonotonicTimeNSec
      let deadline = now + 5000000000
      let path = directory </> T.unpack identity ++ ".wav"
      waitOpen path deadline

waitOpen :: FilePath -> Word64 -> IO (Result Archive)
waitOpen path deadline = do
  opened <- openDescriptor path
  case opened of
    Left problem -> pure (Left problem)
    Right (Just descriptor) -> waitHeader descriptor deadline `onException` closeFd descriptor
    Right Nothing -> retryOpen path deadline

retryOpen :: FilePath -> Word64 -> IO (Result Archive)
retryOpen path deadline = do
  now <- getMonotonicTimeNSec
  if now >= deadline then pure (Left (failure "recording_unavailable")) else threadDelay 100000 >> waitOpen path deadline

waitHeader :: Fd -> Word64 -> IO (Result Archive)
waitHeader descriptor deadline = do
  header <- pread descriptor 0 44
  case header of
    Left problem -> closeFd descriptor >> pure (Left problem)
    Right bytes | B.length bytes == 44 -> startArchive descriptor bytes
    Right _ -> retryHeader descriptor deadline

retryHeader :: Fd -> Word64 -> IO (Result Archive)
retryHeader descriptor deadline = do
  now <- getMonotonicTimeNSec
  if now >= deadline then closeFd descriptor >> pure (Left (failure "recording_unavailable")) else threadDelay 100000 >> waitHeader descriptor deadline

startArchive :: Fd -> ByteString -> IO (Result Archive)
startArchive descriptor header = case validateWav header of
  Left problem -> closeFd descriptor >> pure (Left problem)
  Right () -> do
    initial <- observe descriptor (ArchiveState S.empty 0 False)
    current <- newMVar initial
    worker <- async (poll descriptor current)
    pure (Right (Archive descriptor current worker))

closeRecording :: Archive -> IO ()
closeRecording archive = do
  cancel (archivePoller archive)
  void (ioResult "recording_close_failed" (closeFd (archiveFd archive)))

readClip :: Archive -> Double -> Double -> IO (Result ByteString)
readClip archive start end
  | isNaN start || isNaN end || isInfinite start || isInfinite end || end <= start || end - start > 600 = pure (Left (failure "recording_invalid_interval"))
  | otherwise = do
      now <- getMonotonicTimeNSec
      ready <- waitSamples archive (end + 0.2) (now + 5000000000)
      let bounds = ready >>= (\state -> clipBounds state start end)
      either (pure . Left) (uncurry (readExcerpt (archiveFd archive))) bounds

readExcerpt :: Fd -> Int -> Int -> IO (Result ByteString)
readExcerpt descriptor offset size = do
  result <- pread descriptor offset size
  pure (result >>= complete)
  where
    complete :: ByteString -> Result ByteString
    complete bytes
      | B.length bytes == size = Right bytes
      | otherwise = Left (failure "recording_incomplete_excerpt")

waitSamples :: Archive -> Double -> Word64 -> IO (Result ArchiveState)
waitSamples archive end deadline = do
  current <- readMVar (archiveCurrent archive)
  now <- getMonotonicTimeNSec
  case current of
    Left problem -> pure (Left problem)
    Right state | ready state -> pure (Right state)
    Right _ | now >= deadline -> pure (Left (failure "recording_audio_not_flushed"))
    Right _ -> threadDelay 100000 >> waitSamples archive end deadline
  where
    ready :: ArchiveState -> Bool
    ready state = archiveFinalized state || case S.viewr (archiveAnchors state) of
      EmptyR -> False
      _ :> anchor -> anchorTime anchor >= end
