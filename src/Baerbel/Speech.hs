{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Speech
  ( Resampler, withResampler, resample, resetResampler, splitFrames, decodePCM, encodePCM
  , audiblePCM, speechRequest, transcriptionConfig
  ) where

import Baerbel.Error (Result, failure)
import Control.Exception (IOException, bracket, catch, onException)
import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Bits ((.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.Text (Text)
import Foreign (FunPtr, Ptr, alloca, allocaArray, nullPtr, peek, peekArray, withArray)
import Foreign.C.Types (CDouble(..), CFloat(..), CSize(..), CUInt(..))
import Foreign.C.String (CString)
import System.Posix.DynamicLinker (DL, RTLDFlags(RTLD_NOW), dlopen, dlclose, dlsym)

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
  | otherwise = catch run unavailable
  where
    run = bracket acquire release use
    acquire = openResampler inputRate outputRate
    release = either (const (pure ())) closeResampler
    use = either (pure . Left) action
    unavailable :: IOException -> IO (Result a)
    unavailable _ = pure (Left (failure "audio_resampler_unavailable"))

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
  if ptr == nullPtr
    then dlclose lib >> pure (Left (failure "audio_resampler_failed"))
    else pure (Right (Resampler lib ptr (makeProcess processPtr) (makeDelete deletePtr) (makeClear clearPtr) rate pending done))

closeResampler :: Resampler -> IO ()
closeResampler converter = destroy converter (handle converter) >> dlclose (library converter)

resample :: Resampler -> BS.ByteString -> Bool -> IO (Result BS.ByteString)
resample converter bytes final = do
  done <- readIORef (finished converter)
  pending <- readIORef (remainder converter)
  let combined = pending <> bytes
      size = BS.length combined - BS.length combined `mod` 2
      (complete, rest) = BS.splitAt size combined
  if done || (final && not (BS.null rest)) then pure (Left (failure "invalid_pcm_stream")) else do
    writeIORef (remainder converter) rest
    first <- processPCM converter complete False
    lastPCM <- if final then processPCM converter BS.empty True else pure (Right BS.empty)
    writeIORef (finished converter) final
    pure ((<>) <$> first <*> lastPCM)

processPCM :: Resampler -> BS.ByteString -> Bool -> IO (Result BS.ByteString)
processPCM _ bytes False | BS.null bytes = pure (Right BS.empty)
processPCM converter bytes final = case decodePCM bytes of
  Left err -> pure (Left err)
  Right samples -> processBuffer converter samples final

processBuffer :: Resampler -> [Int16] -> Bool -> IO (Result BS.ByteString)
processBuffer converter samples final = withArray floats processInput
  where
    floats = map fromIntegral samples
    count = length samples
    capacity = ceiling (fromIntegral count * ratio converter) + 8192
    processInput input = allocaArray capacity (\output -> processSamples converter input output count capacity final)

processSamples :: Resampler -> Ptr CFloat -> Ptr CFloat -> Int -> Int -> Bool -> IO (Result BS.ByteString)
processSamples converter input output count capacity final = alloca $ \used ->
  alloca (processAllocated converter input output count capacity final used)

processAllocated :: Resampler -> Ptr CFloat -> Ptr CFloat -> Int -> Int -> Bool -> Ptr CSize -> Ptr CSize -> IO (Result BS.ByteString)
processAllocated converter input output count capacity final used written = do
  let source | final && count == 0 = nullPtr
             | otherwise = input
      inputCount = fromIntegral count
      outputCount = fromIntegral capacity
  err <- process converter (handle converter) source inputCount used output outputCount written
  consumed <- peek used
  produced <- peek written
  let producedCount = fromIntegral produced
  samples <- peekArray producedCount output
  let pcm = encodePCM (map clipSample samples)
      result | err /= nullPtr || consumed /= inputCount = Left (failure "audio_resampler_failed")
             | otherwise = Right pcm
  pure result

clipSample :: CFloat -> Int16
clipSample value = fromIntegral bounded
  where
    rounded = round value :: Int
    bounded = max (-32768) (min 32767 rounded)

decodePCM :: BS.ByteString -> Result [Int16]
decodePCM bytes
  | odd (BS.length bytes) = Left (failure "incomplete_pcm_sample")
  | otherwise = Right (pairs (BS.unpack bytes))
  where
    pairs (lo : hi : rest) = let sample = fromIntegral lo .|. shiftL (fromIntegral hi) 8
                            in sample : pairs rest
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

speechRequest :: Text -> Text -> Text -> Text -> Value
speechRequest model voice instructions input = object (base ++ style)
  where
    base = ["model" .= model, "voice" .= voice, "input" .= input, "response_format" .= ("pcm" :: Text)]
    style = ["instructions" .= instructions | instructions /= "" && model `notElem` ["tts-1", "tts-1-hd"]]

transcriptionConfig :: Text -> Value
transcriptionConfig model = object ["type" .= ("session.update" :: Text), "session" .= session]
  where
    delay = ["delay" .= ("low" :: Text) | model == "gpt-live-transcribe"]
    transcription = object (("model" .= model) : delay)
    input = object ["format" .= object ["type" .= ("audio/pcm" :: Text), "rate" .= (24000 :: Int)],
                    "transcription" .= transcription, "turn_detection" .= (Nothing :: Maybe Value)]
    session = object ["type" .= ("transcription" :: Text), "audio" .= object ["input" .= input]]

resetResampler :: Resampler -> IO (Result ())
resetResampler converter = do
  err <- clear converter (handle converter)
  writeIORef (remainder converter) BS.empty
  writeIORef (finished converter) False
  let result | err == nullPtr = Right ()
             | otherwise = Left (failure "audio_resampler_failed")
  pure result
