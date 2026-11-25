
module Baerbel.Runtime.Encoder
  ( module Core, encodeAudio ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Runtime.Effects ( attempt, mapResult, within )
import Control.Concurrent.Async ( concurrently )
import Data.ByteString ( ByteString )
import System.IO ( Handle, hClose )
import System.Process
    ( ProcessHandle,
      proc,
      waitForProcess,
      withCreateProcess,
      CreateProcess(std_err, std_in, std_out),
      StdStream(NoStream, CreatePipe) )
import Baerbel.Core.Encoder as Core
    ( Encoding(..), encodingResult, checkOutput )
import qualified Data.ByteString as Bytes ( hGetContents, hPut )

encodeAudio :: Encoding -> ByteString -> IO (Result ByteString)
encodeAudio encoding input = mapResult (checkOutput encoding) (attempt "note_encoding_failed" limited)
  where
    duration = encodingTimeoutSeconds encoding * 1000000
    process = (proc (encodingExecutable encoding) (encodingArguments encoding))
      {std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream}
    deadline = failure "note_encoding_timed_out"
    limited = within deadline duration (withCreateProcess process (communicate input))

communicate :: ByteString -> Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO (Result ByteString)
communicate input (Just writer) (Just reader) _ process = do
  (_, output) <- concurrently (writeInput writer input) (Bytes.hGetContents reader)
  code <- waitForProcess process
  pure (encodingResult code output)
communicate _ _ _ _ _ = pure (Left (failure "note_encoding_failed"))

writeInput :: Handle -> ByteString -> IO ()
writeInput writer input = Bytes.hPut writer input >> hClose writer
