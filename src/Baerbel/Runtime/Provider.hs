module Baerbel.Runtime.Provider
  ( module Baerbel.Core.Provider, Execute, Durable, Existing, boundedMap ) where

import Baerbel.Core.Provider
import Baerbel.Core.Error (Result)
import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)
import Data.Aeson (Value)
import Data.Text (Text)

type Execute = Text -> Value -> Text -> IO (Result Value)
type Durable = Text -> Text -> Value -> IO (Result Value) -> IO (Result Value)
type Existing = Text -> Text -> Value -> IO (Result (Maybe Value))

boundedMap :: Int -> (a -> IO b) -> [a] -> IO [b]
boundedMap concurrency operation items = do
  sem <- newQSem concurrency
  let run item = bracket_ (waitQSem sem) (signalQSem sem) (operation item)
  mapConcurrently run items
