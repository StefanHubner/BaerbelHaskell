{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Baerbel.ProviderConfig (MapsRules(..), RouteRules, HttpPolicy(..))
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

geocode :: MapsRules -> Send -> Text -> IO (Result [Geocode])
geocode rules send query = fmap (fmap (fromMaybe [] . geoResults)) operation
  where
    params = [textParam "address" query, textParam "language" (mapsLanguage rules), textParam "region" (mapsGeocodeRegion rules)]
    policy = HttpPolicy (mapsHttpBytes rules) (mapsHttpTimeout rules)
    operation = fetch "maps_invalid_place_response" (get policy send (mapsGeocodeUrl rules) params)

areaBias :: MapsRules -> Geocode -> Bias
areaBias rules geo = case geometryViewport geometry of
  Nothing -> circle
  Just viewport -> let low = point (viewSouthwest viewport); high = point (viewNortheast viewport)
                       width = if longitude high >= longitude low then longitude high - longitude low else longitude high + 360 - longitude low
                   in if latitude low <= latitude high && width <= 180 then Bias Nothing (Just (Rectangle low high)) else circle
  where
    geometry = geoGeometry geo
    center = point (geometryLocation geometry)
    circle = Bias (Just (Circle center (mapsBiasRadius rules))) Nothing

placeCandidate :: MapsRules -> (Text -> Text) -> GooglePlace -> Result (Place, Candidate)
placeCandidate rules redact value = validateCandidate rules place candidate
  where
    label = fromMaybe (googleFormattedAddress value) (googleDisplayName value >>= labelText)
    name = if Text.null label then address else Text.take (mapsNameChars rules) (redact label)
    address = Text.take (mapsAddressChars rules) (redact (googleFormattedAddress value))
    place = Place (googleId value) name address (googleLocation value)
    approximate = any (`elem` mapsApproximateTypes rules) (fromMaybe [] (googleTypes value))
    precision = if approximate then "STREET_OR_AREA_LOCATION" else "PLACE_LOCATION"
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
      save current = pure (current {storedPlaces = insertBounded (mapsReferenceLimit rules) ident place (storedPlaces current)}, result)
  modifyMVar places save

lookupArea :: MapsRules -> (Text -> Text) -> Send -> Places -> Text -> IO (Result [Geocode])
lookupArea rules redact send state text = do
  current <- readMVar state
  let key = Text.toCaseFold text
  maybe fetchArea (pure . Right . pure) (Map.lookup key (fst (storedAreas current)))
  where
    fetchArea = do
      response <- geocode rules send text
      either (pure . Left) validate response
    validate [value] | geoPartialMatch value /= Just True && not (validViewport value) = pure (Left (failure "maps_invalid_place_response"))
    validate values = either (pure . Left) (const (remember values)) (traverse (geoCandidate rules redact) (take (mapsCandidateLimit rules) values))
    remember values@[value] | geoPartialMatch value /= Just True = do
      let key = Text.toCaseFold text
          save current = pure (current {storedAreas = insertBounded (mapsAreaLimit rules) key value (storedAreas current)}, Right values)
      modifyMVar state save
    remember values = pure (Right values)

findPlace :: MapsRules -> (Text -> Text) -> Send -> Places -> FindArgs -> IO (Result Value)
findPlace rules redact send places args
  | Text.toCaseFold area `elem` mapsAmbiguousAreas rules = pure (Right (toJSON (NeedsContext "error" "needs_context" (mapsLocationPrompt rules) Nothing)))
  | invalid (findQuery args) (mapsQueryChars rules) || invalid (findArea args) (mapsAreaChars rules) = pure (Left (failure "maps_invalid_arguments"))
  | otherwise = do
      found <- lookupArea rules redact send places area
      either (pure . Left) search found
  where
    area = Text.strip (findArea args)
    query = Text.strip (findQuery args)
    textQuery = query <> ", " <> area
    invalid text cap = Text.null (Text.strip text) || Text.length text > cap || Text.any (\char -> char < ' ' || char == '\DEL') text
    search values = either (pure . Left) (searchValidated values) (traverse (geoCandidate rules redact) (take (mapsCandidateLimit rules) values))
    searchValidated [value] _ | geoPartialMatch value /= Just True = searchArea value
    searchValidated _ candidates = pure (Right (toJSON (NeedsContext "error" "needs_context" (mapsAreaPrompt rules) (Just (map areaCandidate candidates)))))
    areaCandidate (_, candidate) = AreaCandidate (candidateAddress candidate) (candidatePartialMatch candidate) (candidatePrecision candidate)
    searchArea value
      | not (validViewport value) = pure (Left (failure "maps_invalid_place_response"))
      | otherwise = do
          let resource = SearchRequest textQuery (mapsLanguage rules) (mapsPlacesRegion rules) (mapsCandidateLimit rules) (areaBias rules value)
              fields = Text.encodeUtf8 (mapsSearchFields rules)
              request = Request "POST" (mapsPlacesUrl rules) [("X-Goog-FieldMask", fields)] [] (JsonBody (toJSON resource)) (mapsHttpBytes rules) (mapsHttpTimeout rules)
          response <- fetch "maps_invalid_place_response" (send request)
          either (pure . Left) (resolve value) response
    resolve value response = case fromMaybe [] (pagePlaces response) of
      [] -> do
        fallback <- geocode rules send textQuery
        either (pure . Left) (finishGeocodes value) fallback
      items -> finish value (maybe False (not . Text.null) (pageNextPageToken response) || length items > mapsCandidateLimit rules) (traverse (placeCandidate rules redact) (take (mapsCandidateLimit rules) items))
    finishGeocodes value items = finish value (length items > mapsCandidateLimit rules) (traverse (geoCandidate rules redact) (take (mapsCandidateLimit rules) items))
    finish value more = either (pure . Left) (saveAll value more)
    saveAll value more candidates = do
      saved <- traverse (saveCandidate rules places) candidates
      let count = length saved
          result = FindResult "ok" saved count more (count > 0) (Text.take (mapsAddressChars rules) (redact (geoFormattedAddress value))) "Google Maps" True
      pure (Right (toJSON result))

placeDetails :: MapsRules -> (Text -> Text) -> Send -> IO UTCTime -> Text -> Place -> IO (Result Value)
placeDetails rules redact send clock reference place = do
  response <- fetch "maps_invalid_place_response" (send request)
  now <- clock
  pure (response >>= finish now)
  where
    encoded = Text.decodeUtf8 (urlEncode True (Text.encodeUtf8 (placeId place)))
    url = mapsDetailsUrl rules <> encoded
    fields = Text.encodeUtf8 (mapsDetailsFields rules)
    params = [textParam "languageCode" (mapsLanguage rules), textParam "regionCode" (mapsPlacesRegion rules)]
    request = Request "GET" url [("X-Goog-FieldMask", fields)] params EmptyBody (mapsHttpBytes rules) (mapsHttpTimeout rules)
    finish now value
      | detailId value /= placeId place = Left (failure "maps_place_details_mismatch")
      | otherwise = do
          offset <- traverse (decodeValue "maps_invalid_hours_response") (detailUtcOffsetMinutes value)
          if maybe False (\minutes -> abs minutes >= mapsOffsetLimit rules) offset then Left (failure "maps_invalid_hours_response") else Right ()
          rawCurrent <- traverse (decodeValue "maps_invalid_hours_response") (detailCurrentOpeningHours value)
          rawRegular <- traverse (decodeValue "maps_invalid_hours_response") (detailRegularOpeningHours value)
          current <- openingHours rules redact rawCurrent
          regular <- openingHours rules redact rawRegular
          let schedules = [current, regular]
              available = any (maybe False hoursAvailable) schedules
              reason = if available then Nothing else Just "opening_hours_not_provided"
              status = if available then "available" else "unavailable"
              output = Details "ok" reference name address "Google Maps Places API" fetched status (Nullable current) (Nullable regular)
                (Nullable business) (Nullable zone) (Nullable offset) (Nullable phone) (Nullable number) True reason
          Right (toJSON output)
      where
        label = fromMaybe "" (detailDisplayName value >>= labelText)
        name = if Text.null label then placeLabel place else Text.take (mapsNameChars rules) (redact label)
        address = maybe (placeAddress place) (Text.take (mapsAddressChars rules) . redact) (detailFormattedAddress value)
        fetched = isoUtc now
        phone = Text.take (mapsPhoneChars rules) . redact <$> detailInternationalPhoneNumber value
        number = phone >>= canonical
        business = detailBusinessStatus value >>= parseMaybe parseJSON >>= \status -> if status `elem` mapsBusinessStatuses rules then Just status else Nothing
        zone = Text.take (mapsZoneChars rules) . redact . zoneId <$> (detailTimeZone value >>= parseMaybe parseJSON)

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
  "maps_find_place" -> checked "maps_invalid_arguments" (findPlace rules redact send places) arguments
  "maps_place_details" -> checked "maps_invalid_arguments" details arguments
  "maps_route" -> checked "maps_invalid_arguments" directions arguments
  _ -> pure (Left (failure "maps_unsupported_tool"))
  where
    details args = do
      known <- fmap (fst . storedPlaces) (readMVar places)
      maybe (pure (Left (failure "maps_unknown_place_reference"))) (placeDetails rules redact send clock (detailsPlaceRef args)) (Map.lookup (detailsPlaceRef args) known)
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
  if maybe False truthy (apiError envelope) then Left (failure "maps_api_error") else check (apiStatus envelope)
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
