module Baerbel.Runtime.Effects
  ( continue, mapResult, attempt, within, foldResults, runAll, withResult, monotonicSeconds ) where

import Baerbel.Runtime.Error ( Failure, Result, ioResult )
import Control.Exception ( bracket )
import Control.Monad ( join )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import GHC.Clock ( getMonotonicTimeNSec )
import System.Timeout ( timeout )

-- Pure decisions enter IO only at these explicit execution boundaries.
continue :: (a -> IO (Result b)) -> Result a -> IO (Result b)
continue = either (pure . Left)

mapResult :: (a -> Result b) -> IO (Result a) -> IO (Result b)
mapResult parse = fmap (>>= parse)

attempt :: Text -> IO (Result a) -> IO (Result a)
attempt code action = join <$> ioResult code action

within :: Failure -> Int -> IO (Result a) -> IO (Result a)
within issue duration action = fromMaybe (Left issue) <$> timeout duration action

foldResults :: (s -> a -> IO (Result s)) -> s -> [a] -> IO (Result s)
foldResults _ state [] = pure (Right state)
foldResults step state (value : rest) = step state value >>= continue advance
  where advance next = foldResults step next rest

runAll :: (a -> IO (Result ())) -> [a] -> IO (Result ())
runAll action = foldResults (const action) ()

withResult :: IO (Result a) -> (a -> IO ()) -> (a -> IO (Result b)) -> IO (Result b)
withResult acquire release use = bracket acquire (mapM_ release) (continue use)

monotonicSeconds :: IO Double
monotonicSeconds = fmap ((/ 1000000000) . fromIntegral) getMonotonicTimeNSec
