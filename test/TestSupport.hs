module TestSupport (withRule, withRules) where

import Baerbel.Config (Global, User(..), resolveSettings, rule)
import Baerbel.Error (Result)
import Baerbel.JSON (decodeBytes)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as Bytes
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec (Expectation, expectationFailure)

withRule :: FromJSON a => Text -> Text -> (a -> Expectation) -> Expectation
withRule section key run = do
  bytes <- Bytes.readFile "config/global.json"
  let configuration = decodeBytes "config" bytes :: Result Global
      settings = fmap (\global -> resolveSettings global (User Map.empty)) configuration
      selected = settings >>= \value -> rule value section key
  either (expectationFailure . show) run selected

withRules :: FromJSON a => Text -> (a -> Expectation) -> Expectation
withRules = withRule "providers"
