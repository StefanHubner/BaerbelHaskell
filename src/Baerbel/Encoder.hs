module Baerbel.Encoder (Encoding(..), encodeAudio) where

import Baerbel.Error (Result, failure)
import Baerbel.Effects (attempt, mapResult, within)
import Baerbel.JSON (jsonOptions)
import Control.Concurrent.Async (concurrently)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Process

data Encoding = Encoding
  { encodingExecutable :: FilePath, encodingArguments :: [String]
  , encodingTimeoutSeconds :: Int, encodingMaximumBytes :: Int } deriving (Generic)
instance FromJSON Encoding where parseJSON = genericParseJSON (jsonOptions 8)

encodeAudio :: Encoding -> ByteString -> IO (Result ByteString)
encodeAudio encoding input = mapResult check (attempt "note_encoding_failed" limited)
  where
    duration = encodingTimeoutSeconds encoding * 1000000
    process = (proc (encodingExecutable encoding) (encodingArguments encoding))
      {std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream}
    deadline = failure "note_encoding_timed_out"
    limited = within deadline duration (withCreateProcess process (communicate input))
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
