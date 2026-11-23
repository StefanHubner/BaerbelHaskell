module Baerbel.Error (Failure (..), Result, failure, ioResult) where

import Control.Exception (IOException, try)
import Data.Text (Text)

data Failure = Failure
  { failureCode :: Text
  , failureUncertain :: Bool
  } deriving (Eq, Show)

type Result a = Either Failure a

failure :: Text -> Failure
failure code = Failure code False

ioResult :: Text -> IO a -> IO (Result a)
ioResult code action = convert <$> try action
  where
  convert :: Either IOException a -> Result a
  convert = either (const (Left (failure code))) Right
