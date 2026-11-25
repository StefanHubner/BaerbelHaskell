module TestSupport (withRule, withRules) where

import Baerbel.Core.Config (User(..), resolveSettings, rule)
import Data.Aeson (FromJSON)
import Baerbel.Runtime.Config (loadGlobal)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec (Expectation, expectationFailure)

withRule :: FromJSON a => Text -> Text -> (a -> Expectation) -> Expectation
withRule section key run = do
  configuration <- loadGlobal "config/global.json"
  let settings = fmap (\global -> resolveSettings global (User Map.empty)) configuration
      selected = settings >>= \value -> rule value section key
  either (expectationFailure . show) run selected

withRules :: FromJSON a => Text -> (a -> Expectation) -> Expectation
withRules = withRule "providers"
