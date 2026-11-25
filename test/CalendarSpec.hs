module CalendarSpec (spec) where

import Baerbel.Runtime.Calendar (calendar)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.HTTP (Body(..), Request(..))
import Baerbel.Core.JSON (decodeBytes, jsonOptions)
import Baerbel.Core.Provider (combine)
import Baerbel.Core.ProviderConfig (CalendarRules(..))
import Baerbel.Core.TimeZones (timeZones)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (newIORef, atomicModifyIORef', readIORef, modifyIORef')
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
  where
    rejected :: Failure -> Value
    rejected issue = toJSON (Rejected status (failureCode issue) False)
      where status | failureUncertain issue = "unknown"
                   | otherwise = "error"

spec :: Spec
spec = describe "Calendar tool equivalence" $ do
  it "rejects invalid tool input before requests or ledger access" (withRules "calendar" invalidBoundary)
  it "uses identical normalized arguments for receipt lookup and submission" (withRules "calendar" normalizedCreation)
  it "keeps an invalid create receipt uncertain after submission" (withRules "calendar" invalidReceipt)
  fixtures <- runIO loadCases
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  where fixtureExample fixture = it (Text.unpack (caseLabel fixture)) (withRules "calendar" (`runCase` fixture))


creationArguments :: Text -> Text -> [Text] -> Value
creationArguments start end attendees = object
  [ "summary" .= ("Meeting" :: Text), "start" .= start, "end" .= end
  , "description" .= ("" :: Text), "location" .= ("" :: Text)
  , "attendees" .= attendees, "allow_conflicts" .= True ]

invalidBoundary :: CalendarRules -> Expectation
invalidBoundary rules = do
  effects <- newIORef ([] :: [Text])
  let record text = modifyIORef' effects (<> [text])
      send _ = record "request" >> pure (Left (failure "unexpected_request"))
      durable _ _ _ action = record "reserve" >> action
      existing _ _ _ = record "lookup" >> pure (Right Nothing)
      execute = calendar rules send durable existing timeZones
      search = object ["start_date" .= ("2026-11-26" :: Text), "end_date" .= ("2026-11-25" :: Text), "query" .= ("" :: Text), "calendar" .= ("" :: Text)]
      create = creationArguments "2026-11-25T11:00:00Z" "2026-11-25T10:00:00Z" []
  execute "calendar_agenda" (object ["date" .= ("not-a-day" :: Text)]) "day" `shouldReturn` Left (failure "invalid_calendar_date")
  execute "calendar_search" search "search" `shouldReturn` Left (failure "invalid_calendar_date_range")
  execute "calendar_create" create "create" `shouldReturn` Left (failure "calendar_end_must_follow_start")
  readIORef effects `shouldReturn` []

normalizedCreation :: CalendarRules -> Expectation
normalizedCreation rules = do
  received <- newIORef ([] :: [Value])
  let record value = modifyIORef' received (<> [value])
      existing _ _ value = record value >> pure (Right Nothing)
      durable _ _ value action = record value >> action
      send request = pure (confirmed request)
      execute = calendar rules send durable existing timeZones
      args = creationArguments "2026-11-25T11:00:00+02:00" "2026-11-25T12:00:00+02:00" ["person@EXAMPLE.TEST", "person@example.test"]
      expected = creationArguments "2026-11-25T09:00:00+00:00" "2026-11-25T10:00:00+00:00" ["person@example.test"]
  result <- execute "calendar_create" args "normalized-id"
  result `shouldSatisfy` either (const False) (const True)
  readIORef received `shouldReturn` [expected, expected]

confirmed :: Request -> Result Value
confirmed request = case requestBody request of
  JsonBody resource -> Right (combine [resource, object ["status" .= ("confirmed" :: Text)]])
  _ -> Left (failure "unexpected_request")

invalidReceipt :: CalendarRules -> Expectation
invalidReceipt rules = do
  reserved <- newIORef (0 :: Int)
  let durable _ _ _ action = modifyIORef' reserved (+ 1) >> action
      existing _ _ _ = pure (Right Nothing)
      send request = pure (fmap changeEnd (confirmed request))
      changeEnd (Object fields) = Object (KeyMap.insert "end" (object ["dateTime" .= ("2026-11-25T12:00:00Z" :: Text)]) fields)
      changeEnd value = value
      execute = calendar rules send durable existing timeZones
      args = creationArguments "2026-11-25T09:00:00Z" "2026-11-25T10:00:00Z" []
  execute "calendar_create" args "bad-receipt" `shouldReturn` Left (Failure "calendar_create_response_invalid_receipt" True)
  readIORef reserved `shouldReturn` 1
