module Baerbel.Runtime.Error (Failure (..), Result, failure, ioResult) where

import Baerbel.Core.Error ( Result, Failure(..), failure )
import Control.Exception ( IOException, try )
import Data.Text ( Text )

ioResult :: Text -> IO a -> IO (Result a)
ioResult code action = convert <$> try action
  where
  convert :: Either IOException a -> Result a
  convert = either (const (Left (failure code))) Right
