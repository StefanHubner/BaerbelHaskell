module WindguruSpec (spec) where

import Baerbel.Core.Calendar (TimeZones(..))
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.HTTP (Request(..))
import Baerbel.Core.JSON (decodeBytes, decodeValue, jsonOptions)
import Baerbel.Core.ProviderConfig (WindguruRules(..))
import Baerbel.Core.TimeZones (timeZones)
import Baerbel.Runtime.Windguru (windguru)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import Data.IORef (newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime(..), fromGregorian, addUTCTime)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Test.Hspec
import TestSupport (withRules)

data Reply = Reply {replyQuery :: Text, replyParameters :: Map Text Text, replyBody :: Value, replyError :: Maybe Text} deriving (Generic)
instance FromJSON Reply where parseJSON = genericParseJSON (jsonOptions 5)
data Case = Case
  { caseLabel :: Text, caseTool :: Text, caseArguments :: Value
  , caseResponses :: [Reply], caseSavedSet :: Text, caseExpected :: Value
  , caseExpectedCallCount :: Int } deriving (Generic)
instance FromJSON Case where parseJSON = genericParseJSON (jsonOptions 4)
data Rejected = Rejected {rejectedStatus :: Text, rejectedError :: Text} deriving (Generic)
instance ToJSON Rejected where toJSON = genericToJSON (jsonOptions 8)

loadCases :: IO (Result [Case])
loadCases = fmap (decodeBytes "windguru_fixtures") (Bytes.readFile "test/fixtures/windguru-tools.json")

response :: Case -> Request -> Result Value
response fixture request = maybe (Left (failure "unexpected_request")) resolve selected
  where
    parameters = Map.fromList [(Text.decodeUtf8 key, Text.decodeUtf8 value) | (key, Just value) <- requestQuery request]
    query = Map.findWithDefault "" "q" parameters
    arguments = Map.delete "q" parameters
    selected = find (\reply -> replyQuery reply == query && replyParameters reply == arguments) (caseResponses fixture)
    resolve reply = maybe (Right (replyBody reply)) (Left . failure) (replyError reply)

runCase :: WindguruRules -> Case -> Expectation
runCase rules fixture = do
  calls <- newIORef (0 :: Int)
  let send request = atomicModifyIORef' calls (\count -> (count + 1, ())) >> pure (response fixture request)
      now = UTCTime (fromGregorian 2026 10 15) 28800
      saved = pure (Right (caseSavedSet fixture))
  execute <- windguru rules send (pure now) (zonedInstant timeZones) saved
  result <- execute (caseTool fixture) (caseArguments fixture) "fixture"
  either rejected id result `shouldBe` caseExpected fixture
  readIORef calls `shouldReturn` caseExpectedCallCount fixture
  where rejected issue = toJSON (Rejected "error" (failureCode issue))

spec :: Spec
spec = describe "Windguru tool equivalence" $ do
  fixtures <- runIO loadCases
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> do
      mapM_ fixtureExample cases
      it "retains original retrieval time across cached requests" (withRules "windguru" (cachedTime cases))
      it "keeps completed set members at the shared deadline" (withRules "windguru" (partialDeadline cases))
      it "includes saved-set memory lookup in the deadline" (withRules "windguru" (resolverDeadline cases))
  where fixtureExample fixture = it (Text.unpack (caseLabel fixture)) (withRules "windguru" (`runCase` fixture))


newtype Retrieved = Retrieved {retrievedRetrievedAt :: Text} deriving (Eq, Show, Generic)
instance FromJSON Retrieved where parseJSON = genericParseJSON (jsonOptions 9)
data BatchFacts = BatchFacts
  { batchSuccessfulForecasts :: Int, batchFailedForecasts :: Int, batchDeadlineReached :: Bool } deriving (Eq, Show, Generic)
instance FromJSON BatchFacts where parseJSON = genericParseJSON (jsonOptions 5)

selectFixture :: Text -> [Case] -> (Case -> Expectation) -> Expectation
selectFixture name cases run = maybe (expectationFailure "missing timing fixture") run (find ((== name) . caseTool) cases)

cachedTime :: [Case] -> WindguruRules -> Expectation
cachedTime cases rules = selectFixture "windguru_forecast" cases run
  where
    run fixture = do
      clock <- newIORef (UTCTime (fromGregorian 2026 10 15) 28800)
      calls <- newIORef (0 :: Int)
      let send request = atomicModifyIORef' calls (\count -> (count + 1, ())) >> pure (response fixture request)
          saved = pure (Right (caseSavedSet fixture))
      execute <- windguru rules send (readIORef clock) (zonedInstant timeZones) saved
      first <- execute (caseTool fixture) (caseArguments fixture) "first"
      writeIORef clock (addUTCTime 120 (UTCTime (fromGregorian 2026 10 15) 28800))
      second <- execute (caseTool fixture) (caseArguments fixture) "second"
      (second >>= decodeValue "retrieved") `shouldBe` (first >>= decodeValue "retrieved" :: Result Retrieved)
      readIORef calls `shouldReturn` 2

partialDeadline :: [Case] -> WindguruRules -> Expectation
partialDeadline cases rules = selectFixture "windguru_forecast_set" cases run
  where
    run fixture = do
      let bounded = rules {windguruHttpTimeout = 1}
          now = pure (UTCTime (fromGregorian 2026 10 15) 28800)
          saved = pure (Right (caseSavedSet fixture))
          send request = delay request >> pure (response fixture request)
      execute <- windguru bounded send now (zonedInstant timeZones) saved
      first <- getMonotonicTimeNSec
      result <- execute (caseTool fixture) (caseArguments fixture) "deadline"
      lastTime <- getMonotonicTimeNSec
      (result >>= decodeValue "batch") `shouldBe` Right (BatchFacts 2 1 True)
      (lastTime - first) `shouldSatisfy` (< 1800000000)
    delay request = when (lookup "id_model" (requestQuery request) == Just (Just "3")) (threadDelay 3000000)

resolverDeadline :: [Case] -> WindguruRules -> Expectation
resolverDeadline cases rules = selectFixture "windguru_forecast_set" cases run
  where
    run fixture = do
      let bounded = rules {windguruHttpTimeout = 1}
          now = pure (UTCTime (fromGregorian 2026 10 15) 28800)
          saved = threadDelay 3000000 >> pure (Right (caseSavedSet fixture))
          send _ = pure (Left (failure "unexpected_request"))
      execute <- windguru bounded send now (zonedInstant timeZones) saved
      result <- execute (caseTool fixture) (caseArguments fixture) "resolver"
      result `shouldBe` Left (failure "windguru_timeout")
