module Baerbel.Encoder (Encoding(..), encodeAudio) where

import Baerbel.Error (Result, failure, ioResult)
import Baerbel.JSON (jsonOptions)
import Control.Concurrent.Async (concurrently)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Process
import System.Timeout (timeout)

data Encoding = Encoding
  { encodingExecutable :: FilePath, encodingArguments :: [String]
  , encodingTimeoutSeconds :: Int, encodingMaximumBytes :: Int } deriving (Generic)
instance FromJSON Encoding where parseJSON = genericParseJSON (jsonOptions 8)

encodeAudio :: Encoding -> ByteString -> IO (Result ByteString)
encodeAudio encoding input = do
  outcome <- ioResult "note_encoding_failed" limited
  pure (outcome >>= finish)
  where
    duration = encodingTimeoutSeconds encoding * 1000000
    process = (proc (encodingExecutable encoding) (encodingArguments encoding))
      {std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream}
    limited = timeout duration (withCreateProcess process (communicate input))
    finish Nothing = Left (failure "note_encoding_timed_out")
    finish (Just result) = result >>= check
    check output
      | Bytes.null output || Bytes.length output > encodingMaximumBytes encoding = Left (failure "note_encoding_failed")
      | otherwise = Right output

communicate :: ByteString -> Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO (Result ByteString)
communicate input (Just writer) (Just reader) _ process = do
  (_, output) <- concurrently (writeInput writer input) (Bytes.hGetContents reader)
  code <- waitForProcess process
  pure (encodingResult code output)
communicate _ _ _ _ _ = pure (Left (failure "note_encoding_failed"))

encodingResult :: ExitCode -> ByteString -> Result ByteString
encodingResult ExitSuccess output = Right output
encodingResult (ExitFailure _) _ = Left (failure "note_encoding_failed")

writeInput :: Handle -> ByteString -> IO ()
writeInput writer input = Bytes.hPut writer input >> hClose writer
