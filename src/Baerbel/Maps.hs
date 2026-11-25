{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Baerbel.Maps (maps, placeDetails) where

import Baerbel.TimeFormat (isoUtc)
import Baerbel.Cache (insertBounded)
import Baerbel.Error (Result, failure)
import Baerbel.HTTP (Request(..), Body(..), Send)
import Baerbel.JSON (argumentOptions, camelOptions, jsonOptions, decodeValue)
import Baerbel.OpeningHours (Hours, openingHours, hoursAvailable)
import Baerbel.MapTypes
import Baerbel.MapsRoute (route)
import Baerbel.Provider
import Baerbel.ProviderConfig (MapsRules(..), RouteRules)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import Crypto.Random (getRandomBytes)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Types.URI (urlEncode)

data FindArgs = FindArgs {findQuery :: Text, findArea :: Text} deriving (Generic)
instance FromJSON FindArgs where parseJSON = genericParseJSON (argumentOptions 4)
data RouteArgs = RouteArgs {routeOriginRef :: Text, routeDestinationRef :: Text} deriving (Generic)
instance FromJSON RouteArgs where parseJSON = genericParseJSON (argumentOptions 5)
newtype DetailsArgs = DetailsArgs {detailsPlaceRef :: Text} deriving (Generic)
instance FromJSON DetailsArgs where parseJSON = genericParseJSON (argumentOptions 7)
newtype Label = Label {labelText :: Maybe Text} deriving (Generic)
instance FromJSON Label where parseJSON = genericParseJSON (camelOptions 5)
data GooglePlace = GooglePlace
  { googleId :: Text, googleDisplayName :: Maybe Label, googleFormattedAddress :: Text
  , googleLocation :: Point, googleTypes :: Maybe [Text] } deriving (Generic)
instance FromJSON GooglePlace where parseJSON = genericParseJSON (camelOptions 6)
data PlacePage = PlacePage {pagePlaces :: Maybe [GooglePlace], pageNextPageToken :: Maybe Text} deriving (Generic)
instance FromJSON PlacePage where
  parseJSON = withObject "place page" (\value -> PlacePage <$> value .:! "places" <*> value .:? "nextPageToken")
data Viewport = Viewport {viewSouthwest :: Coordinate, viewNortheast :: Coordinate} deriving (Generic)
instance FromJSON Viewport where parseJSON = genericParseJSON (camelOptions 4)
data Geometry = Geometry
  { geometryLocation :: Coordinate, geometryLocationType :: Maybe Text, geometryViewport :: Maybe Viewport } deriving (Generic)
instance FromJSON Geometry where parseJSON = genericParseJSON (jsonOptions 8)
data Geocode = Geocode
  { geoPlaceId :: Text, geoFormattedAddress :: Text, geoGeometry :: Geometry, geoPartialMatch :: Maybe Bool } deriving (Generic)
instance FromJSON Geocode where parseJSON = genericParseJSON (jsonOptions 3)
newtype GeoPage = GeoPage {geoResults :: Maybe [Geocode]} deriving (Generic)
instance FromJSON GeoPage where
  parseJSON = withObject "geocoding page" (\value -> GeoPage <$> value .:! "results")
data Circle = Circle {circleCenter :: Point, circleRadius :: Int} deriving (Generic)
instance ToJSON Circle where toJSON = genericToJSON (camelOptions 6)
data Rectangle = Rectangle {rectLow :: Point, rectHigh :: Point} deriving (Generic)
instance ToJSON Rectangle where toJSON = genericToJSON (camelOptions 4)
data Bias = Bias {biasCircle :: Maybe Circle, biasRectangle :: Maybe Rectangle} deriving (Generic)
instance ToJSON Bias where toJSON = genericToJSON ((camelOptions 4) {omitNothingFields = True})
data SearchRequest = SearchRequest
  { searchTextQuery :: Text, searchLanguageCode :: Text, searchRegionCode :: Text
  , searchPageSize :: Int, searchLocationBias :: Bias } deriving (Generic)
instance ToJSON SearchRequest where toJSON = genericToJSON (camelOptions 6)
data Candidate = Candidate
  { candidateRef :: Text, candidateName :: Text, candidateAddress :: Text, candidateSource :: Text
  , candidatePrecision :: Text, candidatePartialMatch :: Nullable Bool, candidateApproximate :: Bool
  , candidateWarning :: Maybe Text } deriving (Generic)
instance ToJSON Candidate where toJSON = genericToJSON ((jsonOptions 9) {omitNothingFields = True})
data FindResult = FindResult
  { resultStatus :: Text, resultCandidates :: [Candidate], resultResultCount :: Int
  , resultHasMore :: Bool, resultSelectionRequired :: Bool, resultResolvedArea :: Text
  , resultSource :: Text, resultUntrustedPlaceContent :: Bool } deriving (Generic)
instance ToJSON FindResult where toJSON = genericToJSON (jsonOptions 6)
newtype ZoneId = ZoneId {zoneId :: Text} deriving (Generic)
instance FromJSON ZoneId where parseJSON = genericParseJSON (camelOptions 4)
data DetailResponse = DetailResponse
  { detailId :: Text, detailDisplayName :: Maybe Label, detailFormattedAddress :: Maybe Text
  , detailInternationalPhoneNumber :: Maybe Text, detailBusinessStatus :: Maybe Value
  , detailTimeZone :: Maybe Value, detailUtcOffsetMinutes :: Maybe Value
  , detailCurrentOpeningHours :: Maybe Value, detailRegularOpeningHours :: Maybe Value } deriving (Generic)
instance FromJSON DetailResponse where
  parseJSON = withObject "place details" (\value -> DetailResponse <$> value .: "id" <*> value .:! "displayName"
    <*> value .:! "formattedAddress" <*> value .:! "internationalPhoneNumber" <*> value .:? "businessStatus"
    <*> value .:? "timeZone" <*> value .:! "utcOffsetMinutes" <*> value .:? "currentOpeningHours" <*> value .:? "regularOpeningHours")
data Details = Details
  { outputStatus :: Text, outputPlaceRef :: Text, outputName :: Text, outputAddress :: Text
  , outputSource :: Text, outputFetchedAt :: Text, outputOpeningHoursStatus :: Text
  , outputCurrentOpeningHours :: Nullable Hours, outputRegularOpeningHours :: Nullable Hours
  , outputBusinessStatus :: Nullable Text, outputTimeZone :: Nullable Text, outputUtcOffsetMinutes :: Nullable Int
  , outputInternationalPhoneNumber :: Nullable Text, outputPhoneNumberE164 :: Nullable Text
  , outputUntrustedPlaceContent :: Bool, outputReason :: Maybe Text } deriving (Generic)
instance ToJSON Details where toJSON = genericToJSON ((jsonOptions 6) {omitNothingFields = True})

data MapState = MapState
  { storedPlaces :: (Map Text Place, [Text]), storedAreas :: (Map Text Geocode, [Text]) }
type Places = MVar MapState

data AreaCandidate = AreaCandidate {areaAddress :: Text, areaPartialMatch :: Nullable Bool, areaPrecision :: Text} deriving (Generic)
instance ToJSON AreaCandidate where toJSON = genericToJSON (jsonOptions 4)
data NeedsContext = NeedsContext
  { contextStatus :: Text, contextError :: Text, contextMessage :: Text, contextAreaCandidates :: Maybe [AreaCandidate] } deriving (Generic)
instance ToJSON NeedsContext where toJSON = genericToJSON ((jsonOptions 7) {omitNothingFields = True})

geocodeRequest :: MapsRules -> Text -> Request
geocodeRequest rules query = Request "GET" (mapsGeocodeUrl rules) [] params EmptyBody (mapsHttpBytes rules) (mapsHttpTimeout rules)
  where
    params = [textParam "address" query, textParam "language" (mapsLanguage rules), textParam "region" (mapsGeocodeRegion rules)]

geocode :: Result Value -> Result [Geocode]
geocode response = fmap (fromMaybe [] . geoResults) (fetch "maps_invalid_place_response" response)

areaBias :: MapsRules -> Geocode -> Bias
areaBias rules geo = case geometryViewport geometry of
  Just viewport | latitude low <= latitude high && width <= 180 -> Bias Nothing (Just (Rectangle low high))
    where
      low = point (viewSouthwest viewport)
      high = point (viewNortheast viewport)
      width
        | longitude high >= longitude low = longitude high - longitude low
        | otherwise = longitude high + 360 - longitude low
  _ -> Bias (Just (Circle center (mapsBiasRadius rules))) Nothing
  where
    geometry = geoGeometry geo
    center = point (geometryLocation geometry)

placeCandidate :: MapsRules -> (Text -> Text) -> GooglePlace -> Result (Place, Candidate)
placeCandidate rules redact value = validateCandidate rules place candidate
  where
    label = fromMaybe (googleFormattedAddress value) (googleDisplayName value >>= labelText)
    name
      | Text.null label = address
      | otherwise = Text.take (mapsNameChars rules) (redact label)
    address = Text.take (mapsAddressChars rules) (redact (googleFormattedAddress value))
    place = Place (googleId value) name address (googleLocation value)
    approximate = any (`elem` mapsApproximateTypes rules) (fromMaybe [] (googleTypes value))
    precision
      | approximate = "STREET_OR_AREA_LOCATION"
      | otherwise = "PLACE_LOCATION"
    candidate = Candidate "" name address "google_places" precision (Nullable Nothing) approximate (warning rules approximate)

geoCandidate :: MapsRules -> (Text -> Text) -> Geocode -> Result (Place, Candidate)
geoCandidate rules redact geo = validateCandidate rules place candidate
  where
    address = Text.take (mapsAddressChars rules) (redact (geoFormattedAddress geo))
    geometry = geoGeometry geo
    location = point (geometryLocation geometry)
    precision = case geometryLocationType geometry of
      Just value | value `elem` mapsPrecisions rules -> value
      _ -> "UNKNOWN"
    partial = fromMaybe False (geoPartialMatch geo)
    approximate = partial || precision /= "ROOFTOP"
    place = Place (geoPlaceId geo) address address location
    candidate = Candidate "" address address "google_geocoding" precision (Nullable (Just partial)) approximate (warning rules approximate)

validateCandidate :: MapsRules -> Place -> Candidate -> Result (Place, Candidate)
validateCandidate rules place candidate
  | Text.null ident || Text.length ident > mapsIdentifierChars rules || not (Text.all identifierChar ident) = invalid
  | Text.null (Text.strip (placeLabel place)) || Text.null (Text.strip (placeAddress place)) = invalid
  | abs (latitude location) > 90 || abs (longitude location) > 180 = invalid
  | otherwise = Right (place, candidate)
  where
    ident = placeId place
    location = placePoint place
    invalid = Left (failure "maps_invalid_place_response")
    identifierChar char = isAsciiLower char || isAsciiUpper char || isDigit char || char `elem` ("_-" :: String)

warning :: MapsRules -> Bool -> Maybe Text
warning _ False = Nothing
warning rules True = Just (mapsApproximateWarning rules)

saveCandidate :: MapsRules -> Places -> (Place, Candidate) -> IO Candidate
saveCandidate rules places (place, candidate) = do
  bytes <- getRandomBytes (mapsReferenceBytes rules) :: IO ByteString
  let ident = "place_" <> Text.decodeUtf8 (Base64.encode bytes)
      result = candidate {candidateRef = ident}
      save current = (current {storedPlaces = insertBounded (mapsReferenceLimit rules) ident place (storedPlaces current)}, result)
  modifyMVar places (pure . save)

data AreaResolution = ResolvedArea Geocode | AreaChoices [AreaCandidate]

resolveArea :: MapsRules -> (Text -> Text) -> [Geocode] -> Result AreaResolution
resolveArea rules redact values = case values of
  [value] | geoPartialMatch value /= Just True -> do
    valid <- validateAreaViewport value
    _ <- geoCandidate rules redact valid
    Right (ResolvedArea valid)
  _ -> AreaChoices . map areaCandidate <$> traverse (geoCandidate rules redact) selected
  where
    selected = take (mapsCandidateLimit rules) values
    areaCandidate (_, candidate) = AreaCandidate (candidateAddress candidate) (candidatePartialMatch candidate) (candidatePrecision candidate)

validateAreaViewport :: Geocode -> Result Geocode
validateAreaViewport value
  | validViewport value = Right value
  | otherwise = Left (failure "maps_invalid_place_response")

lookupArea :: MapsRules -> (Text -> Text) -> Send -> Places -> Text -> IO (Result AreaResolution)
lookupArea rules redact send state text = do
  current <- readMVar state
  case Map.lookup key (fst (storedAreas current)) of
    Just value -> pure (Right (ResolvedArea value))
    Nothing -> do
      let request = geocodeRequest rules text
      response <- fmap geocode (send request)
      either (pure . Left) remember (response >>= resolveArea rules redact)
  where
    key = Text.toCaseFold text
    remember result@(ResolvedArea value) = modifyMVar state (pure . save)
      where save current = (current {storedAreas = insertBounded (mapsAreaLimit rules) key value (storedAreas current)}, Right result)
    remember result = pure (Right result)

data FindPlan = FindContext NeedsContext | FindQuery Text Text

planFind :: MapsRules -> FindArgs -> Result FindPlan
planFind rules args
  | Text.toCaseFold area `elem` mapsAmbiguousAreas rules = Right (FindContext (NeedsContext "error" "needs_context" (mapsLocationPrompt rules) Nothing))
  | invalid (findQuery args) (mapsQueryChars rules) || invalid (findArea args) (mapsAreaChars rules) = Left (failure "maps_invalid_arguments")
  | otherwise = Right (FindQuery area (Text.strip (findQuery args) <> ", " <> area))
  where
    area = Text.strip (findArea args)
    invalid text cap = Text.null (Text.strip text) || Text.length text > cap || Text.any (\char -> char < ' ' || char == '\DEL') text

findPlace :: MapsRules -> (Text -> Text) -> Send -> Places -> FindArgs -> IO (Result Value)
findPlace rules redact send places args = case planFind rules args of
  Left issue -> pure (Left issue)
  Right (FindContext result) -> pure (Right (toJSON result))
  Right (FindQuery area query) -> do
    found <- lookupArea rules redact send places area
    either (pure . Left) (searchArea rules redact send places query) found

searchRequest :: MapsRules -> Text -> Geocode -> Request
searchRequest rules query area = Request "POST" (mapsPlacesUrl rules) [("X-Goog-FieldMask", fields)] [] body (mapsHttpBytes rules) (mapsHttpTimeout rules)
  where
    resource = SearchRequest query (mapsLanguage rules) (mapsPlacesRegion rules) (mapsCandidateLimit rules) (areaBias rules area)
    body = JsonBody (toJSON resource)
    fields = Text.encodeUtf8 (mapsSearchFields rules)

searchArea :: MapsRules -> (Text -> Text) -> Send -> Places -> Text -> AreaResolution -> IO (Result Value)
searchArea rules _ _ _ _ (AreaChoices candidates) = pure (Right (toJSON result))
  where result = NeedsContext "error" "needs_context" (mapsAreaPrompt rules) (Just candidates)
searchArea rules redact send places query (ResolvedArea area) = do
  let request = searchRequest rules query area
  response <- fmap (fetch "maps_invalid_place_response") (send request)
  either (pure . Left) resolve (response >>= placeCandidates rules redact)
  where
    resolve Nothing = do
      let request = geocodeRequest rules query
      fallback <- fmap geocode (send request)
      either (pure . Left) save (fallback >>= geocodeCandidates rules redact)
    resolve (Just candidates) = save candidates
    save = savePlaces rules redact places area

placeCandidates :: MapsRules -> (Text -> Text) -> PlacePage -> Result (Maybe (Bool, [(Place, Candidate)]))
placeCandidates rules redact response = case fromMaybe [] (pagePlaces response) of
  [] -> Right Nothing
  items -> Just . (more,) <$> traverse (placeCandidate rules redact) selected
    where
      selected = take (mapsCandidateLimit rules) items
      more = maybe False (not . Text.null) (pageNextPageToken response) || length items > mapsCandidateLimit rules

geocodeCandidates :: MapsRules -> (Text -> Text) -> [Geocode] -> Result (Bool, [(Place, Candidate)])
geocodeCandidates rules redact items = (more,) <$> traverse (geoCandidate rules redact) selected
  where
    selected = take (mapsCandidateLimit rules) items
    more = length items > mapsCandidateLimit rules

savePlaces :: MapsRules -> (Text -> Text) -> Places -> Geocode -> (Bool, [(Place, Candidate)]) -> IO (Result Value)
savePlaces rules redact places area (more, candidates) = do
  saved <- traverse (saveCandidate rules places) candidates
  pure (Right (toJSON (findResult rules redact area more saved)))

findResult :: MapsRules -> (Text -> Text) -> Geocode -> Bool -> [Candidate] -> FindResult
findResult rules redact area more candidates = FindResult "ok" candidates count more (count > 0) address "Google Maps" True
  where
    count = length candidates
    address = Text.take (mapsAddressChars rules) (redact (geoFormattedAddress area))


detailsRequest :: MapsRules -> Place -> Request
detailsRequest rules place = Request "GET" url [("X-Goog-FieldMask", fields)] params EmptyBody (mapsHttpBytes rules) (mapsHttpTimeout rules)
  where
    encoded = Text.decodeUtf8 (urlEncode True (Text.encodeUtf8 (placeId place)))
    url = mapsDetailsUrl rules <> encoded
    fields = Text.encodeUtf8 (mapsDetailsFields rules)
    params = [textParam "languageCode" (mapsLanguage rules), textParam "regionCode" (mapsPlacesRegion rules)]

placeDetails :: MapsRules -> (Text -> Text) -> UTCTime -> Text -> Place -> DetailResponse -> Result Value
placeDetails rules redact now reference place value
  | detailId value /= placeId place = Left (failure "maps_place_details_mismatch")
  | otherwise = do
    offset <- traverse (decodeOffset rules) (detailUtcOffsetMinutes value)
    rawCurrent <- traverse (decodeValue "maps_invalid_hours_response") (detailCurrentOpeningHours value)
    rawRegular <- traverse (decodeValue "maps_invalid_hours_response") (detailRegularOpeningHours value)
    current <- openingHours rules redact rawCurrent
    regular <- openingHours rules redact rawRegular
    let (status, reason) = hoursStatus [current, regular]
        output = Details "ok" reference name address "Google Maps Places API" (isoUtc now) status (Nullable current) (Nullable regular)
          (Nullable business) (Nullable zone) (Nullable offset) (Nullable phone) (Nullable number) True reason
    Right (toJSON output)
  where
    label = fromMaybe "" (detailDisplayName value >>= labelText)
    name
      | Text.null label = placeLabel place
      | otherwise = Text.take (mapsNameChars rules) (redact label)
    address = maybe (placeAddress place) (Text.take (mapsAddressChars rules) . redact) (detailFormattedAddress value)
    phone = Text.take (mapsPhoneChars rules) . redact <$> detailInternationalPhoneNumber value
    number = phone >>= canonical
    business = case detailBusinessStatus value >>= parseMaybe parseJSON of
      Just status | status `elem` mapsBusinessStatuses rules -> Just status
      _ -> Nothing
    zone = Text.take (mapsZoneChars rules) . redact . zoneId <$> (detailTimeZone value >>= parseMaybe parseJSON)

decodeOffset :: MapsRules -> Value -> Result Int
decodeOffset rules value = decodeValue "maps_invalid_hours_response" value >>= valid
  where
    valid minutes
      | abs minutes >= mapsOffsetLimit rules = Left (failure "maps_invalid_hours_response")
      | otherwise = Right minutes

hoursStatus :: [Maybe Hours] -> (Text, Maybe Text)
hoursStatus schedules
  | any (maybe False hoursAvailable) schedules = ("available", Nothing)
  | otherwise = ("unavailable", Just "opening_hours_not_provided")

canonical :: Text -> Maybe Text
canonical supplied = case Text.uncons value of
  Just ('+', digits) | Text.length digits >= 2 && Text.length digits <= 15 && Text.all isDigit digits && not ("0" `Text.isPrefixOf` digits) -> Just value
  _ -> Nothing
  where value = Text.filter (`notElem` (" ().-\t\r\n" :: String)) supplied

maps :: MapsRules -> RouteRules -> (Text -> Text) -> Send -> Bool -> IO UTCTime -> IO Execute
maps rules routeRules redact send elevation clock = do
  places <- newMVar (MapState (Map.empty, []) (Map.empty, []))
  pure (execute rules routeRules redact (apiSend rules send) elevation clock places)

execute :: MapsRules -> RouteRules -> (Text -> Text) -> Send -> Bool -> IO UTCTime -> Places -> Execute
execute rules routeRules redact send elevation clock places name arguments _ = case name of
  "maps_find_place" -> either (pure . Left) (findPlace rules redact send places) (decodeValue "maps_invalid_arguments" arguments)
  "maps_place_details" -> either (pure . Left) details (decodeValue "maps_invalid_arguments" arguments)
  "maps_route" -> either (pure . Left) directions (decodeValue "maps_invalid_arguments" arguments)
  _ -> pure (Left (failure "maps_unsupported_tool"))
  where
    details args = do
      known <- fmap (fst . storedPlaces) (readMVar places)
      maybe (pure (Left (failure "maps_unknown_place_reference"))) (retrieve (detailsPlaceRef args)) (Map.lookup (detailsPlaceRef args) known)
    retrieve reference place = do
      let request = detailsRequest rules place
      response <- send request
      now <- clock
      pure (fetch "maps_invalid_place_response" response >>= placeDetails rules redact now reference place)
    directions args = do
      known <- fmap (fst . storedPlaces) (readMVar places)
      let origin = Map.lookup (routeOriginRef args) known
          destination = Map.lookup (routeDestinationRef args) known
          result = route routeRules send elevation <$> origin <*> destination
      fromMaybe (pure (Left (failure "maps_unknown_place_reference"))) result


data ApiEnvelope = ApiEnvelope {apiStatus :: Maybe Value, apiError :: Maybe Value} deriving (Generic)
instance FromJSON ApiEnvelope where parseJSON = genericParseJSON (camelOptions 3)

apiSend :: MapsRules -> Send -> Send
apiSend rules send request = fmap (>>= apiResponse rules) (send request)

apiResponse :: MapsRules -> Value -> Result Value
apiResponse rules value = do
  envelope <- decodeValue "maps_invalid_response" value
  case apiError envelope of
    Just issue | truthy issue -> Left (failure "maps_api_error")
    _ -> check (apiStatus envelope)
  where
    check Nothing = Right value
    check (Just (String status)) | status `elem` ["OK", "ZERO_RESULTS"] = Right value
    check (Just (String status)) | status `elem` mapsApiStatuses rules = Left (failure ("maps_api_" <> Text.toLower status))
    check _ = Left (failure "maps_api_error")


validViewport :: Geocode -> Bool
validViewport value = maybe True valid (geometryViewport (geoGeometry value))
  where
    valid viewport = all coordinate [viewSouthwest viewport, viewNortheast viewport]
    coordinate location = abs (coordinateLat location) <= 90 && abs (coordinateLng location) <= 180
