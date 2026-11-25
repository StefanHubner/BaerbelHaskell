{-# LANGUAGE ForeignFunctionInterface #-}
module Baerbel.VAD (VAD, withVAD, resetVAD, isSpeech) where

import Baerbel.Error (Result, failure)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)

newtype VAD = VAD (Ptr ())

foreign import ccall unsafe "WebRtcVad_Create" create :: Ptr (Ptr ()) -> IO CInt
foreign import ccall unsafe "WebRtcVad_Init" initialize :: Ptr () -> IO CInt
foreign import ccall unsafe "WebRtcVad_set_mode" setMode :: Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "WebRtcVad_Free" release :: Ptr () -> IO ()
foreign import ccall unsafe "WebRtcVad_Process" process :: Ptr () -> CInt -> Ptr Int16 -> CInt -> IO CInt

withVAD :: Int -> (VAD -> IO (Result a)) -> IO (Result a)
withVAD mode action = bracket acquire cleanup use
  where
  acquire = openVAD mode
  cleanup = either (const (pure ())) (\(VAD pointer) -> release pointer)
  use = either (pure . Left) action

openVAD :: Int -> IO (Result VAD)
openVAD mode = alloca createAt
  where
  createAt location = do
    status <- create location
    created location status
  created location 0 = peek location >>= configure mode
  created _ _ = pure (Left (failure "vad_unavailable"))

configure :: Int -> Ptr () -> IO (Result VAD)
configure mode pointer = do
  result <- resetVAD (VAD pointer) mode
  either (\issue -> release pointer >> pure (Left issue)) (const (pure (Right (VAD pointer)))) result

resetVAD :: VAD -> Int -> IO (Result ())
resetVAD (VAD pointer) mode = do
  initialized <- initialize pointer
  selected <- setMode pointer selectedMode
  pure (initializedVAD initialized selected)
  where selectedMode = fromIntegral mode

initializedVAD :: CInt -> CInt -> Result ()
initializedVAD 0 0 = Right ()
initializedVAD _ _ = Left (failure "vad_initialization_failed")

isSpeech :: VAD -> Int -> ByteString -> IO (Result Bool)
isSpeech (VAD pointer) rate audio = BS.useAsCStringLen audio classify
  where
  classify (bytes, size) = do
    let samples = fromIntegral (size `div` 2)
        sampleRate = fromIntegral rate
        buffer = castPtr bytes
    status <- process pointer sampleRate buffer samples
    pure (decision status)
  decision 0 = Right False
  decision 1 = Right True
  decision _ = Left (failure "vad_frame_failed")
