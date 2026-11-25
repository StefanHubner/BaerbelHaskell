{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Speech
  ( module Core, Resampler, withResampler, resample, resetResampler ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Runtime.Effects ( attempt, continue, withResult )
import Control.Exception ( onException )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Data.Int ( Int16 )
import Foreign
    ( FunPtr,
      Ptr,
      alloca,
      allocaArray,
      nullPtr,
      peek,
      peekArray,
      withArray )
import Foreign.C.String ( CString )
import Foreign.C.Types
    ( CDouble(..), CFloat(..), CSize(..), CUInt(..) )
import System.Posix.DynamicLinker
    ( DL, RTLDFlags(RTLD_NOW), dlopen, dlclose, dlsym )
import Baerbel.Core.Speech as Core
    ( preparePCM,
      clipSample,
      decodePCM,
      decodeSamples,
      encodePCM,
      splitFrames,
      audiblePCM )
import qualified Data.ByteString as BS ( ByteString, empty )

type Create = CDouble -> CDouble -> CUInt -> Ptr CString -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())
type Process = Ptr () -> Ptr CFloat -> CSize -> Ptr CSize -> Ptr CFloat -> CSize -> Ptr CSize -> IO CString
type Delete = Ptr () -> IO ()
type Clear = Ptr () -> IO CString
foreign import ccall "dynamic" makeCreate :: FunPtr Create -> Create
foreign import ccall "dynamic" makeProcess :: FunPtr Process -> Process
foreign import ccall "dynamic" makeDelete :: FunPtr Delete -> Delete
foreign import ccall "dynamic" makeClear :: FunPtr Clear -> Clear

data Resampler = Resampler
  { library :: DL, handle :: Ptr (), process :: Process, destroy :: Delete
  , clear :: Clear, ratio :: Double, remainder :: IORef BS.ByteString, finished :: IORef Bool
  }

-- libsoxr's defaults are float32 mono I/O and HQ filtering, matching Python.
withResampler :: Int -> Int -> (Resampler -> IO (Result a)) -> IO (Result a)
withResampler inputRate outputRate action
  | inputRate <= 0 || outputRate <= 0 = pure (Left (failure "invalid_sample_rate"))
  | otherwise = attempt "audio_resampler_unavailable" run
  where
    acquire = openResampler inputRate outputRate
    run = withResult acquire closeResampler action

openResampler :: Int -> Int -> IO (Result Resampler)
openResampler inputRate outputRate = do
  lib <- dlopen "libsoxr.so.0" [RTLD_NOW]
  onException (loadResampler lib inputRate outputRate) (dlclose lib)

loadResampler :: DL -> Int -> Int -> IO (Result Resampler)
loadResampler lib inputRate outputRate = do
  createPtr <- dlsym lib "soxr_create"
  processPtr <- dlsym lib "soxr_process"
  deletePtr <- dlsym lib "soxr_delete"
  clearPtr <- dlsym lib "soxr_clear"
  let input = fromIntegral inputRate
      output = fromIntegral outputRate
      rate = fromIntegral outputRate / fromIntegral inputRate
  pending <- newIORef BS.empty
  done <- newIORef False
  ptr <- makeCreate createPtr input output 1 nullPtr nullPtr nullPtr nullPtr
  let loaded
        | ptr == nullPtr = dlclose lib >> pure (Left (failure "audio_resampler_failed"))
        | otherwise = pure (Right (Resampler lib ptr (makeProcess processPtr) (makeDelete deletePtr) (makeClear clearPtr) rate pending done))
  loaded

closeResampler :: Resampler -> IO ()
closeResampler converter = destroy converter (handle converter) >> dlclose (library converter)

resample :: Resampler -> BS.ByteString -> Bool -> IO (Result BS.ByteString)
resample converter bytes final = do
  done <- readIORef (finished converter)
  pending <- readIORef (remainder converter)
  let prepared = preparePCM done final pending bytes
  continue (uncurry (processChunk converter final)) prepared

processChunk :: Resampler -> Bool -> [Int16] -> BS.ByteString -> IO (Result BS.ByteString)
processChunk converter final samples rest = do
  writeIORef (remainder converter) rest
  first <- case samples of
    [] -> pure (Right BS.empty)
    _ -> processBuffer converter samples False
  lastPCM <- flush
  writeIORef (finished converter) final
  pure ((<>) <$> first <*> lastPCM)
  where
    flush | final = processBuffer converter [] True
          | otherwise = pure (Right BS.empty)

processBuffer :: Resampler -> [Int16] -> Bool -> IO (Result BS.ByteString)
processBuffer converter samples final = withArray floats processInput
  where
    floats = map fromIntegral samples
    count = length samples
    capacity = ceiling (fromIntegral count * ratio converter) + 8192
    processInput input = allocaArray capacity (\output -> processSamples converter input output count capacity final)

processSamples :: Resampler -> Ptr CFloat -> Ptr CFloat -> Int -> Int -> Bool -> IO (Result BS.ByteString)
processSamples converter input output count capacity final = alloca $ \used -> alloca $ \written -> do
  err <- process converter (handle converter) source inputCount used output outputCount written
  consumed <- peek used
  produced <- peek written
  let producedCount = fromIntegral produced
  samples <- peekArray producedCount output
  pure (processAllocated inputCount err consumed samples)
  where
    source | final && count == 0 = nullPtr
           | otherwise = input
    inputCount = fromIntegral count
    outputCount = fromIntegral capacity

processAllocated :: CSize -> CString -> CSize -> [CFloat] -> Result BS.ByteString
processAllocated inputCount err consumed samples
  | err /= nullPtr || consumed /= inputCount = Left (failure "audio_resampler_failed")
  | otherwise = Right pcm
  where pcm = encodePCM (map clipSample samples)

resetResampler :: Resampler -> IO (Result ())
resetResampler converter = do
  err <- clear converter (handle converter)
  writeIORef (remainder converter) BS.empty
  writeIORef (finished converter) False
  let result | err == nullPtr = Right ()
             | otherwise = Left (failure "audio_resampler_failed")
  pure result
