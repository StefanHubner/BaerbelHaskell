module LiveCheck.Check
  ( CheckResult(..), check, skip, ensure, metered, measured, emit, successful ) where

import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Runtime.Effects (attempt, monotonicSeconds, within)
import Baerbel.Runtime.HTTP (Send)
import Data.Aeson (ToJSON(..), Value(..), encode, genericToJSON, object, (.=))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as Bytes
import qualified Data.Text.IO as Text

data CheckResult = CheckResult
  { checkName :: Text, checkStatus :: Text, checkSeconds :: Double
  , checkDetails :: Value } deriving (Generic)

instance ToJSON CheckResult where toJSON = genericToJSON (jsonOptions 5)

check :: Text -> IO (Result Value) -> IO CheckResult
check name action = do
  Text.putStrLn ("Checking " <> name)
  (seconds, result) <- measured action
  let report = outcome name seconds result
  emit report
  pure report

measured :: IO (Result a) -> IO (Double, Result a)
measured action = do
  start <- monotonicSeconds
  let limited = within (failure "live_check_timeout") 180000000 action
  result <- attempt "live_check_io_failed" limited
  finish <- monotonicSeconds
  pure (finish - start, result)

outcome :: Text -> Double -> Result Value -> CheckResult
outcome name seconds (Right detail) = CheckResult name "passed" seconds detail
outcome name seconds (Left issue) = CheckResult name "failed" seconds detail
  where detail = object ["error" .= failureCode issue, "uncertain" .= failureUncertain issue]

skip :: Text -> Text -> IO CheckResult
skip name reason = emit result >> pure result
  where result = CheckResult name "disabled" 0 (String reason)

ensure :: Text -> Bool -> Result ()
ensure _ True = Right ()
ensure reason False = Left (failure reason)

metered :: Send -> IO (Send, IO Int)
metered sender = do
  count <- newIORef 0
  let send request = atomicModifyIORef' count increment >> sender request
  pure (send, readIORef count)
  where increment total = (total + 1, ())

emit :: ToJSON a => a -> IO ()
emit = Bytes.putStrLn . encode

successful :: CheckResult -> Bool
successful result = checkStatus result /= "failed"
