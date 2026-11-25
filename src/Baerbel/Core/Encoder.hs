module Baerbel.Core.Encoder where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( jsonOptions )
import Data.Aeson ( FromJSON(..), genericParseJSON )
import Data.ByteString ( ByteString )
import GHC.Generics ( Generic )
import System.Exit ( ExitCode(..) )
import qualified Data.ByteString as Bytes ( length, null )

data Encoding = Encoding
  { encodingExecutable :: FilePath, encodingArguments :: [String]
  , encodingTimeoutSeconds :: Int, encodingMaximumBytes :: Int } deriving (Generic)
instance FromJSON Encoding where parseJSON = genericParseJSON (jsonOptions 8)

encodingResult :: ExitCode -> ByteString -> Result ByteString
encodingResult ExitSuccess output = Right output
encodingResult (ExitFailure _) _ = Left (failure "note_encoding_failed")


checkOutput :: Encoding -> ByteString -> Result ByteString
checkOutput encoding output
  | Bytes.null output || Bytes.length output > encodingMaximumBytes encoding = Left (failure "note_encoding_failed")
  | otherwise = Right output
