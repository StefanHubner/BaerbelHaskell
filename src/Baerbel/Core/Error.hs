module Baerbel.Core.Error where

import Data.Text ( Text )

data Failure = Failure
  { failureCode :: Text
  , failureUncertain :: Bool
  } deriving (Eq, Show)

type Result a = Either Failure a

failure :: Text -> Failure
failure code = Failure code False
