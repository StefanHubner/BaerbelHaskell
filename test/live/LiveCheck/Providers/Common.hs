{-# LANGUAGE TupleSections #-}
module LiveCheck.Providers.Common
  ( tool, ensure, member, firstItem, successful, timed, noMutation, noReceipt ) where

import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (decodeValue, field, textField)
import Baerbel.Runtime.Effects (mapResult)
import Baerbel.Runtime.Provider (Execute, Durable, Existing)
import Data.Aeson (FromJSON, Value)
import Data.Text (Text)
import LiveCheck.Check (ensure, measured)

tool :: Execute -> Text -> Value -> IO (Result Value)
tool execute name args = mapResult successful (execute name args "local-live-read")

successful :: Value -> Result Value
successful value
  | textField "status" value == Just "ok" = Right value
  | otherwise = Left (failure "live_provider_result_not_ok")

member :: FromJSON a => Text -> Value -> Result a
member key value = maybe missing (decodeValue "live_provider_result_invalid") (field key value)
  where missing = Left (failure "live_provider_result_missing_field")

firstItem :: [a] -> Result a
firstItem (value : _) = Right value
firstItem [] = Left (failure "live_provider_result_empty")

timed :: IO (Result a) -> IO (Result (a, Double))
timed action = do
  (seconds, result) <- measured action
  pure ((, seconds) <$> result)

noMutation :: Durable
noMutation _ _ _ _ = pure (Left (failure "live_probe_mutation_forbidden"))

noReceipt :: Existing
noReceipt _ _ _ = pure (Left (failure "live_probe_mutation_forbidden"))
