module MapsSpec (spec) where

import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Request(..))
import Baerbel.JSON (decodeBytes, decodeValue, jsonOptions)
import Baerbel.MapTypes (Place(..), Point(..))
import Baerbel.Maps (maps, placeDetails)
import Baerbel.MapsRoute (route)
import Baerbel.ProviderConfig (RouteRules, MapsRules)
import Data.Aeson hiding (Result)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as Bytes
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime(..), fromGregorian)
import GHC.Generics (Generic)
import Test.Hspec
import TestSupport (withRules)

data Reply = Reply {replyUrl :: Text, replyBody :: Value} deriving (Generic)
instance FromJSON Reply where parseJSON = genericParseJSON (jsonOptions 5)
data Location = Location
  { locationPlaceId :: Maybe Text, locationLatitude :: Double, locationLongitude :: Double
  , locationLabel :: Text } deriving (Generic)
instance FromJSON Location where parseJSON = genericParseJSON (jsonOptions 8)
data Case = Case
  { caseLabel :: Text, caseOrigin :: Location, caseDestination :: Location
  , caseElevation :: Bool, caseResponses :: [Reply], caseExpected :: Value
  , caseExpectedCallCount :: Int } deriving (Generic)
instance FromJSON Case where parseJSON = genericParseJSON (jsonOptions 4)
data Rejected = Rejected {rejectedStatus :: Text, rejectedError :: Text} deriving (Generic)
instance ToJSON Rejected where toJSON = genericToJSON (jsonOptions 8)

loadCases :: IO (Result [Case])
loadCases = fmap (decodeBytes "maps_fixtures") (Bytes.readFile "test/fixtures/maps-routes.json")

place :: Location -> Place
place value = Place ident (locationLabel value) "" point
  where
    ident = fromMaybe "" (locationPlaceId value)
    point = Point (locationLatitude value) (locationLongitude value)

runCase :: RouteRules -> Case -> Expectation
runCase rules fixture = do
  replies <- newIORef (caseResponses fixture)
  calls <- newIORef (0 :: Int)
  let response request = atomicModifyIORef' replies (consume request)
      send request = atomicModifyIORef' calls (\count -> (count + 1, ())) >> response request
  result <- route rules send (caseElevation fixture) (place (caseOrigin fixture)) (place (caseDestination fixture))
  either rejected id result `shouldBe` caseExpected fixture
  readIORef calls `shouldReturn` caseExpectedCallCount fixture
  where
    rejected issue = toJSON (Rejected "error" (failureCode issue))
    consume request (reply : rest) | requestUrl request == replyUrl reply = (rest, Right (replyBody reply))
    consume _ rest = (rest, Left (failure "unexpected_request"))

spec :: Spec
spec = do
  referenceSpec
  findSpec
  detailSpec
  routeSpec

referenceSpec :: Spec
referenceSpec = describe "Maps reference boundary" $
  it "rejects unknown references before HTTP or clock effects" $
    withRules "maps" (withRules "route" . unknownReferences)

unknownReferences :: MapsRules -> RouteRules -> Expectation
unknownReferences rules routeRules = do
  calls <- newIORef (0 :: Int)
  let record = atomicModifyIORef' calls (\count -> (count + 1, ()))
      send _ = record >> pure (Left (failure "unexpected_request"))
      clock = record >> pure (UTCTime (fromGregorian 2026 10 15) 28800)
      details = object ["place_ref" .= ("unknown" :: Text)]
      directions = object ["origin_ref" .= ("unknown" :: Text), "destination_ref" .= ("other" :: Text)]
      rejected = Left (failure "maps_unknown_place_reference")
  execute <- maps rules routeRules id send False clock
  execute "maps_place_details" details "details" `shouldReturn` rejected
  execute "maps_route" directions "route" `shouldReturn` rejected
  readIORef calls `shouldReturn` 0

routeSpec :: Spec
routeSpec = describe "Maps route equivalence" $ do
  fixtures <- runIO loadCases
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  where fixtureExample fixture = it (Text.unpack (caseLabel fixture)) (withRules "route" (`runCase` fixture))


data DetailsCase = DetailsCase {detailLabel :: Text, detailBody :: Value, detailExpected :: Value} deriving (Generic)
instance FromJSON DetailsCase where parseJSON = genericParseJSON (jsonOptions 6)

loadDetails :: IO (Result [DetailsCase])
loadDetails = fmap (decodeBytes "maps_details_fixtures") (Bytes.readFile "test/fixtures/maps-details.json")

runDetails :: MapsRules -> DetailsCase -> Expectation
runDetails rules fixture = do
  let now = UTCTime (fromGregorian 2026 10 15) 28800
      selected = Place "fixture-place" "Saved label" "Saved address" (Point 51 0)
      redact = Text.replace "fixture-key" "[redacted]"
      result = decodeValue "maps_invalid_place_response" (detailBody fixture) >>= placeDetails rules redact now "place_fixture" selected
  either rejected id result `shouldBe` detailExpected fixture
  where rejected issue = toJSON (Rejected "error" (failureCode issue))

detailSpec :: Spec
detailSpec = describe "Maps place details equivalence" $ do
  fixtures <- runIO loadDetails
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  where fixtureExample fixture = it (Text.unpack (detailLabel fixture)) (withRules "maps" (`runDetails` fixture))


data FindCase = FindCase
  { findLabel :: Text, findArguments :: Value, findResponses :: [Value]
  , findExpected :: Value, findExpectedCallCount :: Int } deriving (Generic)
instance FromJSON FindCase where parseJSON = genericParseJSON (jsonOptions 4)

loadFindCases :: IO (Result [FindCase])
loadFindCases = fmap (decodeBytes "maps_find_fixtures") (Bytes.readFile "test/fixtures/maps-find.json")

withoutReferences :: Value -> Value
withoutReferences (Object values) = Object (fmap withoutReferences (KeyMap.delete "ref" values))
withoutReferences (Array values) = Array (fmap withoutReferences values)
withoutReferences value = value

runFind :: MapsRules -> RouteRules -> FindCase -> Expectation
runFind rules routeRules fixture = do
  replies <- newIORef (findResponses fixture)
  calls <- newIORef (0 :: Int)
  let send _ = atomicModifyIORef' calls (\count -> (count + 1, ())) >> atomicModifyIORef' replies consume
      now = pure (UTCTime (fromGregorian 2026 10 15) 28800)
  execute <- maps rules routeRules id send False now
  result <- execute "maps_find_place" (findArguments fixture) "fixture"
  withoutReferences (either rejected id result) `shouldBe` findExpected fixture
  readIORef calls `shouldReturn` findExpectedCallCount fixture
  where
    rejected issue = toJSON (Rejected "error" (failureCode issue))
    consume (value : rest) = (rest, Right value)
    consume [] = ([], Left (failure "unexpected_request"))

findSpec :: Spec
findSpec = describe "Maps place search equivalence" $ do
  fixtures <- runIO loadFindCases
  case fixtures of
    Left issue -> it "loads baseline fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  where
    fixtureExample fixture = it (Text.unpack (findLabel fixture)) (withRules "maps" (withRoute fixture))
    withRoute fixture rules = withRules "route" (\routeRules -> runFind rules routeRules fixture)
