module ToolsSpec (spec) where

import Baerbel.Cache (memoize)
import Baerbel.Contacts (candidate)
import Baerbel.Error (Result, failure)
import Baerbel.Gmail (bodyText, gmail, summary)
import Baerbel.HTTP (Request(..))
import Baerbel.JSON (decodeBytes, decodeValue, jsonOptions)
import Baerbel.MapTypes (Point(..))
import Baerbel.MapsRoute (heading, gainLoss, samplePath)
import Baerbel.Notes
import qualified Baerbel.Profile as Profile
import Baerbel.Profile (ProfileState(..), ProfileStore(..), emptyProfile, transition, parseCommand, profile)
import Baerbel.ProviderConfig (ContactsRules, GmailRules, ProfileRules, RouteRules)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian)
import GHC.Generics (Generic)
import qualified GmailSpec
import qualified MapsSpec
import qualified CalendarSpec
import qualified WindguruSpec
import TestSupport (withRule, withRules)
import Test.Hspec

data Fixture = Fixture {fixtureInput :: Value, fixtureExpected :: Value} deriving (Generic)
instance FromJSON Fixture where parseJSON = genericParseJSON (jsonOptions 7)
data HeadingInput = HeadingInput {headingOrigin :: (Double, Double), headingDestination :: (Double, Double)} deriving (Generic)
instance FromJSON HeadingInput where parseJSON = genericParseJSON (jsonOptions 7)
data CompletionInput = CompletionInput {completionPhrase :: Text, completionSpeech :: Text} deriving (Generic)
instance FromJSON CompletionInput where parseJSON = genericParseJSON (jsonOptions 10)
data QuoteFragment = QuoteFragment {quoteSequenceNumber :: Int, quoteText :: Text} deriving (Generic)
instance FromJSON QuoteFragment where
  parseJSON = withObject "fragment" (\value -> QuoteFragment <$> value .: "sequence" <*> value .: "text")
data QuoteInput = QuoteInput {quotePhrase :: Text, quoteFragments :: [QuoteFragment]} deriving (Generic)
instance FromJSON QuoteInput where parseJSON = genericParseJSON (jsonOptions 5)

now :: UTCTime
now = UTCTime (fromGregorian 2026 11 19) 0

golden :: FilePath -> (Value -> Result Value) -> Expectation
golden name operation = goldenIO name (pure . operation)

goldenIO :: FilePath -> (Value -> IO (Result Value)) -> Expectation
goldenIO name operation = do
  bytes <- Bytes.readFile ("test/fixtures/" <> name <> ".json")
  let fixtures = decodeBytes "fixture" bytes :: Result [Fixture]
      compareFixture value = operation (fixtureInput value) `shouldReturn` Right (fixtureExpected value)
  either (expectationFailure . show) (mapM_ compareFixture) fixtures

contactsGolden :: ContactsRules -> Expectation
contactsGolden rules = golden "contacts-candidates" operation
  where operation value = fmap (toJSON . candidate rules) (decodeValue "person" value)

bodyGolden :: GmailRules -> Expectation
bodyGolden rules = goldenIO "gmail-bodies" operation
  where operation value = either (pure . Left) (fmap (fmap toJSON) . bodyText rules) (decodeValue "part" value)

summaryGolden :: GmailRules -> Expectation
summaryGolden rules = goldenIO "gmail-summaries" operation
  where operation value = either (pure . Left) (fmap (fmap toJSON) . summary rules) (decodeValue "message" value)

headingGolden :: RouteRules -> Expectation
headingGolden rules = golden "route-headings" operation
  where
    operation value = fmap render (decodeValue "heading" value)
    render input = toJSON (heading rules "fixture" origin destination)
      where
        (lat1, lon1) = headingOrigin input
        (lat2, lon2) = headingDestination input
        origin = Point lat1 lon1
        destination = Point lat2 lon2

gainGolden :: RouteRules -> Expectation
gainGolden rules = golden "route-gain-loss" operation
  where operation value = fmap (toJSON . gainLoss rules) (decodeValue "elevations" value)

completionGolden :: Expectation
completionGolden = golden "note-completion" operation
  where
    operation value = fmap render (decodeValue "completion" value)
    render input = toJSON (completionMatches (completionPhrase input) (completionSpeech input))

quoteGolden :: Expectation
quoteGolden = golden "note-quotes" operation
  where
    operation value = fmap render (decodeValue "quote" value)
    render input = toJSON (quoteSequence (quotePhrase input) (map fragment (quoteFragments input)))
    fragment item = Fragment (quoteSequenceNumber item) (quoteText item) Nothing Nothing

unchangedProfile :: ProfileRules -> Expectation
unchangedProfile rules = case initial of
  Left issue -> expectationFailure (show issue)
  Right (state, _) -> do
    let updated = prepared >>= \command -> transition rules later command state
    fmap (profileUpdatedAt . fst) updated `shouldBe` Right (profileUpdatedAt state)
    fmap (profileValues . fst) updated `shouldBe` Right (Map.singleton "project_2" "value")
  where
    arguments = object ["key" .= ("project_2" :: Text), "value" .= (" value " :: Text)]
    prepared = parseCommand rules "profile_set" arguments
    initial = prepared >>= \command -> transition rules now command emptyProfile
    later = UTCTime (fromGregorian 2026 11 20) 0

readOnlyProfile :: ProfileRules -> Expectation
readOnlyProfile rules = do
  clocks <- newIORef (0 :: Int)
  writes <- newIORef (0 :: Int)
  let state = emptyProfile {profileValues = Map.singleton "home" "Vienna"}
      store = ProfileStore (pure (Right state)) (\_ -> modifyIORef' writes (+ 1) >> pure (Left (failure "unexpected_write")))
      clock = modifyIORef' clocks (+ 1) >> pure now
      arguments = object ["key" .= ("home" :: Text)]
      expected = Profile.snapshot rules state (Just "home")
  profile rules store clock "profile_get" arguments "read" `shouldReturn` Right expected
  readIORef clocks `shouldReturn` 0
  readIORef writes `shouldReturn` 0

invalidSend :: GmailRules -> Expectation
invalidSend rules = do
  reserved <- newIORef (0 :: Int)
  requests <- newIORef ([] :: [Request])
  let durable _ _ _ effect = modifyIORef' reserved (+ 1) >> effect
      send request = modifyIORef' requests (request :) >> pure (Left (failure "unexpected_request"))
      operation = gmail rules send durable (pure now)
      arguments = object ["to" .= ([] :: [Text]), "subject" .= ("x" :: Text), "body" .= ("body" :: Text)]
  result <- operation "gmail_send" arguments "invalid-id"
  result `shouldBe` Left (failure "invalid_recipients")
  readIORef reserved `shouldReturn` 0
  fmap length (readIORef requests) `shouldReturn` 0

cachedRequests :: Expectation
cachedRequests = do
  calls <- newIORef (0 :: Int)
  let operation key = modifyIORef' calls (+ 1) >> pure (Right (key :: Int))
  cached <- memoize 2 300 id operation
  mapM cached [1, 2, 1, 3, 1, 2] `shouldReturn` map Right [1, 2, 1, 3, 1, 2]
  readIORef calls `shouldReturn` 4

cancelledDelivery :: NoteRules -> Expectation
cancelledDelivery rules = do
  snapshot <- newIORef (Transcript [] 0 0 10 True 0 0)
  accepted <- newEmptyMVar
  delivered <- newIORef (0 :: Int)
  let deliver _ _ _ = putMVar accepted () >> threadDelay 10000 >> modifyIORef' delivered (+ 1) >> pure (Right (object []))
      effects = NoteEffects (readIORef snapshot) (\_ _ -> pure (Right "pcm")) deliver
        (pure ()) (pure "note-id") (\_ _ _ -> pure ())
  (execute, _, close) <- notes rules effects
  _ <- execute "owner_note_start" (object ["start_quote" .= ("" :: Text)]) "note-id"
  writeIORef snapshot (Transcript [Fragment 1 "Remember this, send it" (Just 1) (Just 2)] 5 5 10 True 1 0)
  delivery <- Async.async (execute "owner_note_send" (object ["completion_phrase" .= ("send it" :: Text)]) "send-id")
  takeMVar accepted
  Async.cancel delivery
  close
  readIORef delivered `shouldReturn` 1

spec :: Spec
spec = describe "Provider behavior against the unchanged Python implementation" $ do
  GmailSpec.spec
  MapsSpec.spec
  CalendarSpec.spec
  WindguruSpec.spec
  it "preserves contact labels, limits, and extension suppression" (withRules "contacts" contactsGolden)
  it "preserves Gmail text alternatives and attachment metadata" (withRules "gmail" bodyGolden)
  it "preserves decoded headers and single reply recipients" (withRules "gmail" summaryGolden)
  it "preserves compass headings including the antimeridian" (withRules "route" headingGolden)
  it "preserves elevation deadband estimates" (withRules "route" gainGolden)
  it "matches observed completion phrases" completionGolden
  it "finds unique quotes across transcription fragments" quoteGolden
  it "preserves profile no-op timestamps and numeric key components" (withRules "profile" unchangedProfile)
  it "reads profile values without reading the clock or invoking writes" (withRules "profile" readOnlyProfile)
  it "rejects invalid send recipients before reserving or requesting" (withRules "gmail" invalidSend)
  it "uses a bounded call-local LRU cache" cachedRequests
  it "completes accepted delivery after the tool caller is cancelled" (withRule "notes" "selection" cancelledDelivery)
  it "samples the shortest antimeridian path" $ do
    let (points, _) = samplePath 3 [Point 0 179, Point 0 (-179)]
        longitudes = map longitude points
    longitudes `shouldBe` [179, 180, -179]
