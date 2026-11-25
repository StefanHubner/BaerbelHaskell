{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Recording
  ( module Core, Archive, openRecording, closeRecording, readClip ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Runtime.Effects ( continue, mapResult )
import Baerbel.Runtime.Error ( ioResult )
import Control.Concurrent
    ( MVar, modifyMVar, newMVar, readMVar, threadDelay )
import Control.Concurrent.Async ( Async, async, cancel )
import Control.Exception ( onException )
import Control.Monad ( void )
import Data.ByteString ( ByteString )
import Data.Sequence ( ViewR((:>), EmptyR) )
import Data.Text ( Text )
import Data.Word ( Word8, Word64 )
import Foreign.C.Error ( Errno, eNOENT, getErrno )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt(..), CLong(..), CSize(..) )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr ( Ptr, castPtr )
import GHC.Clock ( getMonotonicTimeNSec )
import System.FilePath ( (</>) )
import System.Posix.Files
    ( FileStatus, fileSize, getFdStatus, modificationTimeHiRes )
import System.Posix.IO ( closeFd )
import System.Posix.Types ( CSsize(..), Fd(..) )
import Baerbel.Core.Recording as Core
    ( Interval(..),
      Timeline(..),
      Segment(..),
      ArchiveState(..),
      Anchor(..),
      emptyTimeline,
      addFrame,
      resetTimeline,
      sentSamples,
      consumeFrame,
      transcriptBounds,
      firstMatch,
      lastMatch,
      validateWav,
      littleWord,
      sampleState,
      sampleBounds,
      projectSamples,
      sampleRange,
      clipInterval,
      clipBounds,
      intervalBounds,
      excerptBounds,
      validIdentity,
      readExcerpt )
import qualified Data.ByteString as B ( length, packCStringLen )
import qualified Data.Sequence as S ( empty, viewr )
import qualified Data.Text as T ( unpack )

foreign import ccall safe "open" nativeOpen :: CString -> CInt -> IO CInt
foreign import ccall safe "pread" nativeRead :: CInt -> Ptr Word8 -> CSize -> CLong -> IO CSsize

data Archive = Archive
  { archiveFd :: Fd, archiveCurrent :: MVar (Result ArchiveState), archivePoller :: Async () }

pread :: Fd -> Int -> Int -> IO (Result ByteString)
pread (Fd descriptor) offset count = allocaBytes count readBuffer
  where
    readBuffer :: Ptr Word8 -> IO (Result ByteString)
    readBuffer buffer = do
      size <- nativeRead descriptor buffer (fromIntegral count) (fromIntegral offset)
      copy buffer size
    copy buffer size
      | size < 0 = pure (Left (failure "recording_read_failed"))
      | otherwise = Right <$> B.packCStringLen (castPtr buffer, fromIntegral size)

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
  retry now
  where
    retry now
      | now >= deadline = pure (Left (failure "recording_unavailable"))
      | otherwise = threadDelay 100000 >> waitOpen path deadline

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
  retry now
  where
    retry now
      | now >= deadline = closeFd descriptor >> pure (Left (failure "recording_unavailable"))
      | otherwise = threadDelay 100000 >> waitHeader descriptor deadline

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
readClip archive start end = continue readInterval (clipInterval start end)
  where
    readInterval interval = do
      now <- getMonotonicTimeNSec
      let untilTime = end + 0.2
          deadline = now + 5000000000
          readBounds = mapResult (`intervalBounds` interval) (waitSamples archive untilTime deadline)
      bounds <- readBounds
      continue (\(offset, size) -> mapResult (readExcerpt size) (pread (archiveFd archive) offset size)) bounds

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
