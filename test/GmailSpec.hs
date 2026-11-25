module GmailSpec (spec) where

import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.Gmail (gmailSession)
import Baerbel.HTTP (Request(..))
import Baerbel.JSON (decodeBytes, jsonOptions)
import Baerbel.ProviderConfig (GmailRules)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime(..), fromGregorian)
import GHC.Generics (Generic)
import Test.Hspec
import TestSupport (withRules)

data Reply = Reply {replyUrl :: Text, replyBody :: Value} deriving (Generic)
instance FromJSON Reply where parseJSON = genericParseJSON (jsonOptions 5)
data Case = Case
  { caseLabel :: Text, caseTool :: Text, caseArguments :: Value
  , caseResponses :: [Reply], caseExpected :: Value, caseExpectedCallCount :: Int } deriving (Generic)
instance FromJSON Case where parseJSON = genericParseJSON (jsonOptions 4)
data Rejected = Rejected {rejectedStatus :: Text, rejectedError :: Text} deriving (Generic)
instance ToJSON Rejected where toJSON = genericToJSON (jsonOptions 8)

loadCases :: IO (Result [Case])
loadCases = fmap (decodeBytes "gmail_fixtures") (Bytes.readFile "test/fixtures/gmail-tools.json")

runCase :: GmailRules -> Case -> Expectation
runCase rules fixture = do
  calls <- newIORef (0 :: Int)
  let send request = atomicModifyIORef' calls (\count -> (count + 1, ())) >> pure (response request)
      durable _ _ _ action = action
      now = UTCTime (fromGregorian 2026 10 15) 28800
  execute <- gmailSession rules send durable (pure now)
  result <- execute (caseTool fixture) (caseArguments fixture) "fixture"
  either rejected id result `shouldBe` caseExpected fixture
  readIORef calls `shouldReturn` caseExpectedCallCount fixture
  where
    rejected issue = toJSON (Rejected (failureStatus issue) (failureCode issue))
    response request = maybe (Left (failure "unexpected_request")) (Right . replyBody) (find ((== requestUrl request) . replyUrl) (caseResponses fixture))

failureStatus :: Failure -> Text
failureStatus issue
  | failureUncertain issue = "unknown"
  | otherwise = "error"

spec :: Spec
spec = describe "Gmail tool equivalence" $ do
  fixtures <- runIO loadCases
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  where fixtureExample fixture = it (Text.unpack (caseLabel fixture)) (withRules "gmail" (`runCase` fixture))
