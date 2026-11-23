module CalendarSpec (spec) where

import Baerbel.Calendar (calendar)
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Body(..), Request(..))
import Baerbel.JSON (decodeBytes, jsonOptions)
import Baerbel.Provider (combine)
import Baerbel.ProviderConfig (CalendarRules)
import Baerbel.TimeZones (timeZones)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Test.Hspec
import TestSupport (withRules)

data Reply = Reply {replyUrl :: Text, replyPageToken :: Maybe Text, replyBody :: Maybe Value, replyError :: Maybe Text} deriving (Generic)
instance FromJSON Reply where parseJSON = genericParseJSON (jsonOptions 5)
data Case = Case
  { caseLabel :: Text, caseTool :: Text, caseArguments :: Value
  , caseResponses :: [Reply], casePrevious :: Maybe Value, caseExpected :: Value
  , caseExpectedCallCount :: Int, caseExpectedReserved :: Int } deriving (Generic)
instance FromJSON Case where parseJSON = genericParseJSON (jsonOptions 4)
data Rejected = Rejected {rejectedStatus :: Text, rejectedError :: Text, rejectedRetrySafe :: Bool} deriving (Generic)
instance ToJSON Rejected where toJSON = genericToJSON (jsonOptions 8)

loadCases :: IO (Result [Case])
loadCases = fmap (decodeBytes "calendar_fixtures") (Bytes.readFile "test/fixtures/calendar-tools.json")

response :: Case -> Request -> Result Value
response fixture request
  | requestMethod request == "POST" = case requestBody request of
      JsonBody resource -> Right (combine [resource, object ["status" .= ("confirmed" :: Text)]])
      _ -> Left (failure "unexpected_request_body")
  | otherwise = maybe (Left (failure "unexpected_request")) resolve selected
  where
    token = lookup "pageToken" (requestQuery request) >>= fmap Text.decodeUtf8
    selected = find (\reply -> replyUrl reply == requestUrl request && replyPageToken reply == token) (caseResponses fixture)
    resolve reply = case replyError reply of
      Just code -> Left (failure code)
      Nothing -> maybe (Left (failure "missing_fixture_body")) Right (replyBody reply)

runCase :: CalendarRules -> Case -> Expectation
runCase rules fixture = do
  calls <- newIORef (0 :: Int)
  reserved <- newIORef (0 :: Int)
  let increment ref = atomicModifyIORef' ref (\count -> (count + 1, ()))
      send request = increment calls >> pure (response fixture request)
      durable _ _ _ operation = increment reserved >> operation
      existing _ _ _ = pure (Right (casePrevious fixture))
      execute = calendar rules send durable existing timeZones
  result <- execute (caseTool fixture) (caseArguments fixture) "fixture-calendar"
  either rejected id result `shouldBe` caseExpected fixture
  readIORef calls `shouldReturn` caseExpectedCallCount fixture
  readIORef reserved `shouldReturn` caseExpectedReserved fixture
  where rejected issue = toJSON (Rejected (if failureUncertain issue then "unknown" else "error") (failureCode issue) False)

spec :: Spec
spec = describe "Calendar tool equivalence" $ do
  fixtures <- runIO loadCases
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  where fixtureExample fixture = it (Text.unpack (caseLabel fixture)) (withRules "calendar" (`runCase` fixture))
