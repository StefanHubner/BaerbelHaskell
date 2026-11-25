{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.MapsRoute (route, distance, heading, gainLoss, samplePath) where

import Data.Char (isDigit, isAsciiUpper)
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Body(..), Request(..), Send)
import Baerbel.JSON (camelOptions, jsonOptions)
import Baerbel.MailContent (htmlTextWith)
import Baerbel.MapTypes
import Baerbel.ProviderConfig (RouteRules(..))
import Baerbel.Provider (fetch, firstOr, textParam, combine, referenceByteSize)
import Data.Aeson hiding (Result, pairs)
import Control.Monad (foldM)
import Text.Printf (printf)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data RouteRequest = RouteRequest
  { reqOrigin :: Value, reqDestination :: Value, reqTravelMode :: Text
  , reqLanguageCode :: Text, reqUnits :: Text, reqComputeAlternativeRoutes :: Bool
  , reqPolylineQuality :: Text, reqPolylineEncoding :: Text } deriving (Generic)
instance ToJSON RouteRequest where toJSON = genericToJSON (camelOptions 3)
newtype Location = Location {locationLatLng :: Point} deriving (Generic)
instance FromJSON Location where
  parseJSON = withObject "route location" (\value -> value .: "latLng" >>= withObject "coordinates" coordinates)
    where coordinates value = Location <$> (Point <$> value .:! "latitude" .!= 0 <*> value .:! "longitude" .!= 0)
data Line = Line {lineType :: Text, lineCoordinates :: [[Double]]} deriving (Generic)
instance FromJSON Line where parseJSON = genericParseJSON (camelOptions 4)
newtype Polyline = Polyline {polylineGeoJsonLinestring :: Line} deriving (Generic)
instance FromJSON Polyline where parseJSON = genericParseJSON (camelOptions 8)
data Instruction = Instruction {navInstructions :: Text, navManeuver :: Maybe Text} deriving (Generic)
instance FromJSON Instruction where parseJSON = genericParseJSON (camelOptions 3)
data RawStep = RawStep
  { stepDistanceMeters :: Maybe Double, stepStaticDuration :: Maybe Text
  , stepNavigationInstruction :: Instruction, stepStartLocation :: Location
  , stepEndLocation :: Location, stepPolyline :: Polyline } deriving (Generic)
instance FromJSON RawStep where parseJSON = genericParseJSON (camelOptions 4)
data Leg = Leg {legStartLocation :: Location, legEndLocation :: Location, legSteps :: [RawStep]} deriving (Generic)
instance FromJSON Leg where parseJSON = genericParseJSON (camelOptions 3)
data RawRoute = RawRoute
  { rawDistanceMeters :: Maybe Double, rawDuration :: Maybe Text, rawWarnings :: Maybe [Text]
  , rawLegs :: [Leg] } deriving (Generic)
instance FromJSON RawRoute where
  parseJSON = withObject "route" (\value -> RawRoute <$> value .:! "distanceMeters" <*> value .:! "duration" <*> value .:! "warnings" <*> value .: "legs")
newtype Routes = Routes {responseRoutes :: Maybe [RawRoute]} deriving (Generic)
instance FromJSON Routes where parseJSON = genericParseJSON (camelOptions 8)
data Heading = Heading
  { headCompassDirection :: Maybe Text, headBearingDegrees :: Maybe Double, headDirectionBasis :: Text }
  deriving (Eq, Show, Generic)
instance ToJSON Heading where toJSON = genericToJSON (jsonOptions 4)
data Step = Step
  { outStep :: Int, outInstruction :: Text, outManeuver :: Text, outDistanceM :: Maybe Int
  , outDurationS :: Maybe Double, outCompassDirection :: Maybe Text
  , outBearingDegrees :: Maybe Double, outDirectionBasis :: Text } deriving (Generic)
instance ToJSON Step where toJSON = genericToJSON (jsonOptions 3)
data StepTerrain = StepTerrain
  { stStatus :: Text, stReason :: Maybe Text, stSampleCount :: Maybe Int, stSampledDistanceM :: Maybe Int
  , stEstimatedAscentM :: Maybe Int, stEstimatedDescentM :: Maybe Int, stNetElevationChangeM :: Maybe Int } deriving (Generic)
instance ToJSON StepTerrain where toJSON = genericToJSON ((jsonOptions 2) {omitNothingFields = True})
newtype GroundField = GroundField {groundTerrain :: Maybe StepTerrain} deriving (Generic)
instance ToJSON GroundField where toJSON = genericToJSON ((jsonOptions 6) {omitNothingFields = True})
data DetailedStep = DetailedStep {detailStep :: Step, detailTerrain :: Maybe StepTerrain}
instance ToJSON DetailedStep where toJSON value = combine [toJSON (detailStep value), toJSON (GroundField (detailTerrain value))]
data Terrain = Terrain
  { terrainStatus :: Text, terrainSource :: Text, terrainReason :: Maybe Text
  , terrainSampleCount :: Maybe Int, terrainSampleSpacingM :: Maybe Double
  , terrainMaximumResolutionM :: Maybe Double, terrainEstimatedAscentM :: Maybe Int
  , terrainEstimatedDescentM :: Maybe Int, terrainNetElevationChangeM :: Maybe Int
  , terrainNoiseFilterM :: Maybe Double, terrainLimitations :: Maybe Text } deriving (Generic)
instance ToJSON Terrain where toJSON = genericToJSON ((jsonOptions 7) {omitNothingFields = True})
data RouteResult = RouteResult
  { resultStatus :: Text, resultTravelMode :: Text, resultSource :: Text, resultAttribution :: Text
  , resultOrigin :: Text, resultDestination :: Text, resultDistanceM :: Int, resultDurationS :: Double
  , resultOverallDirection :: Heading, resultCompassNote :: Text, resultSteps :: [DetailedStep]
  , resultStepCount :: Int, resultDirectionsComplete :: Bool, resultWarnings :: [Text]
  , resultUntrustedMapsContent :: Bool, resultTerrain :: Maybe Terrain } deriving (Generic)
instance ToJSON RouteResult where toJSON = genericToJSON ((jsonOptions 6) {omitNothingFields = True})
data Elevation = Elevation {elevationLocation :: Coordinate, elevationElevation :: Double, elevationResolution :: Maybe Double} deriving (Generic)
instance FromJSON Elevation where parseJSON = genericParseJSON (camelOptions 9)
data Elevations = Elevations {elevStatus :: Text, elevResults :: Maybe [Elevation]} deriving (Generic)
instance FromJSON Elevations where parseJSON = genericParseJSON (camelOptions 4)

radians :: Double -> Double
radians value = value * pi / 180

distance :: Point -> Point -> Double
distance first second = 12742000 * asin (sqrt (min 1 (max 0 value)))
  where
    lat1 = radians (latitude first)
    lat2 = radians (latitude second)
    dlat = lat2 - lat1
    dlon = radians (longitude second - longitude first)
    value = sin (dlat / 2) ** 2 + cos lat1 * cos lat2 * sin (dlon / 2) ** 2

heading :: RouteRules -> Text -> Point -> Point -> Heading
heading rules basis first second
  | distance first second < routeHeadingMinimumM rules = Heading Nothing Nothing "insufficient_displacement"
  | otherwise = Heading (Just direction) (Just roundedBearing) basis
  where
    lat1 = radians (latitude first)
    lat2 = radians (latitude second)
    dlon = radians (longitude second - longitude first)
    y = sin dlon * cos lat2
    x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dlon
    angle = atan2 y x * 180 / pi
    bearing
      | angle < 0 = angle + 360
      | otherwise = angle
    index = floor ((bearing + 22.5) / 45) `mod` 8
    labels = ["north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west"]
    direction = firstOr "north" (drop index labels)
    roundedBearing = fromIntegral ((round (bearing * 10) :: Int) `mod` 3600) / 10

lastOr :: a -> [a] -> a
lastOr = foldl' (\_ item -> item)

initialHeading :: RouteRules -> [Point] -> Heading
initialHeading _ [] = Heading Nothing Nothing "insufficient_displacement"
initialHeading rules (first : rest) = heading rules basis first chosen
  where
    candidates = filter ((>= routeHeadingMinimumM rules) . distance first) rest
    chosen = firstOr (lastOr first rest) candidates
    basis = case candidates of
      [] -> "step_start_to_end"
      _ -> "initial_step_path"

geometry :: RouteRules -> RawStep -> Result [Point]
geometry rules step
  | length coordinates > routeGeometryLimit rules = Left (failure "maps_route_too_large")
  | lineType line /= "LineString" || null coordinates = Left (failure "maps_invalid_route_geometry")
  | otherwise = traverse coordinate coordinates
  where
    line = polylineGeoJsonLinestring (stepPolyline step)
    coordinates = lineCoordinates line
    coordinate [lon, lat] | abs lon <= 180 && abs lat <= 90 = Right (Point lat lon)
    coordinate _ = Left (failure "maps_invalid_route_geometry")

duration :: RouteRules -> Text -> Result Double
duration rules text = case readMaybe (Text.unpack raw) of
  Just value | syntax && value >= 0 && value <= routeDurationLimitS rules -> Right (rounded 3 value)
  _ -> Left (failure "maps_invalid_route_response")
  where
    raw = Text.dropEnd 1 text
    parts = Text.splitOn "." raw
    digits part = not (Text.null part) && Text.length part <= 9 && Text.all isDigit part
    syntax = "s" `Text.isSuffixOf` text && length parts <= 2 && all digits parts

rounded :: Int -> Double -> Double
rounded places value = fromIntegral (round (value * factor) :: Integer) / factor
  where factor = 10 ** fromIntegral places

routeText :: Int -> Text -> Result Text
routeText cap value
  | Text.length value > cap * 4 || Text.length cleaned > cap = Left (failure "maps_route_too_large")
  | otherwise = Right cleaned
  where
    plain = htmlTextWith ["script", "style"] ["br", "div", "p", "li"] ["div", "p", "li"] value
    cleaned = Text.filter (\char -> char >= ' ' && char /= '\DEL') (Text.unwords (Text.words plain))

number :: Double -> Double -> Double -> Result Double
number lower upper value
  | isNaN value || isInfinite value || value < lower || value > upper = Left (failure "maps_invalid_route_response")
  | otherwise = Right value

validPoint :: Point -> Result Point
validPoint value
  | abs (latitude value) > 90 || abs (longitude value) > 180 = Left (failure "maps_invalid_route_geometry")
  | otherwise = Right value

normalizeStep :: RouteRules -> (Int, RawStep) -> Result (Step, [Point])
normalizeStep rules (index, value) = do
  points <- geometry rules value
  seconds <- traverse (duration rules) (stepStaticDuration value)
  metres <- traverse (number 0 (routeDistanceLimitM rules)) (stepDistanceMeters value)
  start <- validPoint (locationLatLng (stepStartLocation value))
  end <- validPoint (locationLatLng (stepEndLocation value))
  let nav = stepNavigationInstruction value
  instruction <- routeText (routeInstructionChars rules) (navInstructions nav)
  let first = firstOr start points
      final = lastOr end points
      direction = initialHeading rules points
      maneuver = fromMaybe "MANEUVER_UNSPECIFIED" (navManeuver nav)
      step = Step index instruction maneuver (round <$> metres) seconds (headCompassDirection direction) (headBearingDegrees direction) (headDirectionBasis direction)
  validateStep rules instruction maneuver start end first final
  Right (step, points)

validateStep :: RouteRules -> Text -> Text -> Point -> Point -> Point -> Point -> Result ()
validateStep rules instruction maneuver start end first final
  | Text.null instruction = Left (failure "maps_incomplete_directions")
  | Text.null maneuver || Text.length maneuver > routeManeuverChars rules || not (Text.all (\char -> isAsciiUpper char || char == '_') maneuver) = Left (failure "maps_invalid_route_response")
  | distance start first > routeGeometryToleranceM rules || distance end final > routeGeometryToleranceM rules = Left (failure "maps_invalid_route_geometry")
  | otherwise = Right ()

data Path = Path {pathSteps :: [DetailedStep], pathPoints :: [Point], pathSpans :: [(Double, Double)], pathLength :: Double}

appendStep :: RouteRules -> Path -> (Step, [Point]) -> Result Path
appendStep rules path (step, points)
  | gap > routeGeometryToleranceM rules = Left (failure "maps_discontinuous_route_geometry")
  | length (pathPoints path) + length points > routeGeometryLimit rules = Left (failure "maps_route_too_large")
  | otherwise = Right (Path (pathSteps path <> [DetailedStep step Nothing]) (pathPoints path <> points) (pathSpans path <> [(begin, end)]) end)
  where
    first = firstOr (Point 0 0) points
    previous = lastOr first (pathPoints path)
    gap = distance previous first
    begin = pathLength path + gap
    end = begin + sum (zipWith distance points (drop 1 points))

normalize :: RouteRules -> Place -> Place -> Routes -> Result (RouteResult, Path)
normalize rules origin destination response = case fromMaybe [] (responseRoutes response) of
  [value] -> case rawLegs value of
    [leg] -> normalizeLeg rules origin destination value leg
    _ -> Left (failure "maps_incomplete_directions")
  [] -> Left (failure "maps_no_route")
  _ -> Left (failure "maps_invalid_route_response")

normalizeLeg :: RouteRules -> Place -> Place -> RawRoute -> Leg -> Result (RouteResult, Path)
normalizeLeg rules origin destination value leg
  | null (legSteps leg) = Left (failure "maps_incomplete_directions")
  | length (legSteps leg) > routeStepLimit rules || length warnings > routeWarningLimit rules = Left (failure "maps_route_too_large")
  | otherwise = do
      parts <- traverse (normalizeStep rules) (zip [1..] (legSteps leg))
      path <- foldM (appendStep rules) (Path [] [] [] 0) parts
      summarizeRoute rules origin destination value leg path
  where warnings = fromMaybe [] (rawWarnings value)

summarizeRoute :: RouteRules -> Place -> Place -> RawRoute -> Leg -> Path -> Result (RouteResult, Path)
summarizeRoute rules origin destination value leg path = do
  first <- validPoint (locationLatLng (legStartLocation leg))
  final <- validPoint (locationLatLng (legEndLocation leg))
  validateRoutePath rules value path first final
  seconds <- duration rules (fromMaybe "0s" (rawDuration value))
  metres <- number 0 (routeDistanceLimitM rules) (fromMaybe 0 (rawDistanceMeters value))
  warnings <- traverse (routeText (routeInstructionChars rules)) (fromMaybe [] (rawWarnings value))
  originName <- routeText (routeLabelChars rules) (placeName (routeOriginLabel rules) origin)
  destinationName <- routeText (routeLabelChars rules) (placeName (routeDestinationLabel rules) destination)
  let direction = heading rules "route_start_to_destination_not_initial_heading" first final
      result = RouteResult "ok" "WALK" "Google Maps Routes API" "Google Maps" originName destinationName (round metres) seconds direction
        (routeCompassNote rules) (pathSteps path) (length (pathSteps path)) True warnings True Nothing
  bounded <- routeBudget rules result
  Right (bounded, path)

validateRoutePath :: RouteRules -> RawRoute -> Path -> Point -> Point -> Result ()
validateRoutePath rules value path first final
  | distance first beginning > tolerance || distance final ending > tolerance = Left (failure "maps_invalid_route_geometry")
  | pathLength path >= routeHeadingMinimumM rules && missingTotals = Left (failure "maps_incomplete_directions")
  | otherwise = Right ()
  where
    beginning = firstOr first (pathPoints path)
    ending = lastOr final (pathPoints path)
    tolerance = routeGeometryToleranceM rules
    missingTotals = isNothing (rawDistanceMeters value) || isNothing (rawDuration value)

routeBudget :: ToJSON a => RouteRules -> a -> Result a
routeBudget rules value
  | referenceByteSize value > routeOutputBytes rules = Left (failure "maps_route_too_large")
  | otherwise = Right value

placeName :: Text -> Place -> Text
placeName fallback place
  | not (Text.null (placeLabel place)) = placeLabel place
  | not (Text.null (placeAddress place)) = placeAddress place
  | otherwise = fallback

unavailable :: Text -> Terrain
unavailable reason = Terrain "unavailable" "Google Maps Elevation API" (Just reason) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

samplePath :: Int -> [Point] -> ([Point], [Double])
samplePath count points = (map sample positions, positions)
  where
    pairs = zip points (drop 1 points)
    lengths = map (uncurry distance) pairs
    cumulative = scanl (+) 0 lengths
    total = lastOr 0 cumulative
    positions = [total * fromIntegral index / fromIntegral (count - 1) | index <- [0 .. count - 1]]
    segments = zip3 cumulative lengths pairs
    fallback = firstOr (Point 0 0) points
    sample position = case dropWhile (\(start, size, _) -> start + size < position) segments of
      (_, 0, (first, second)) : _ -> interpolate first second 0
      (start, size, (first, second)) : _ -> interpolate first second ((position - start) / size)
      [] -> lastOr fallback points

interpolate :: Point -> Point -> Double -> Point
interpolate first second fraction = Point lat lon
  where
    lat = latitude first + (latitude second - latitude first) * fraction
    rawDelta = longitude second - longitude first
    delta
      | rawDelta > 180 = rawDelta - 360
      | rawDelta < - 180 = rawDelta + 360
      | otherwise = rawDelta
    rawLon = longitude first + delta * fraction
    lon
      | rawLon > 180 = rawLon - 360
      | rawLon < - 180 = rawLon + 360
      | otherwise = rawLon

type ElevationTrend = (Double, Double, Int, Double, Double)

gainLoss :: RouteRules -> [Double] -> (Double, Double)
gainLoss _ [] = (0, 0)
gainLoss rules (first : rest) = finishTrend (foldl' (advanceTrend rules) (first, first, 0, 0, 0) rest)

advanceTrend :: RouteRules -> ElevationTrend -> Double -> ElevationTrend
advanceTrend rules state@(anchor, extreme, trend, gain, loss) value
  | trend == 0 && abs (value - anchor) >= routeElevationNoiseM rules = (anchor, value, direction, gain, loss)
  | trend == 0 = state
  | trend > 0 && value >= extreme = (anchor, value, trend, gain, loss)
  | trend > 0 && extreme - value >= routeElevationNoiseM rules = (extreme, value, -1, gain + extreme - anchor, loss)
  | trend < 0 && value <= extreme = (anchor, value, trend, gain, loss)
  | trend < 0 && value - extreme >= routeElevationNoiseM rules = (extreme, value, 1, gain, loss + anchor - extreme)
  | otherwise = state
  where
    direction
      | value > anchor = 1
      | otherwise = -1

finishTrend :: ElevationTrend -> (Double, Double)
finishTrend (anchor, extreme, trend, gain, loss)
  | trend > 0 = (gain + extreme - anchor, loss)
  | trend < 0 = (gain, loss + anchor - extreme)
  | otherwise = (gain, loss)

data TerrainPlan = TerrainUnavailable Text | TerrainSamples [Point] [Double] Double

planTerrain :: RouteRules -> Path -> TerrainPlan
planTerrain rules path
  | total < routeTerrainMinimumM rules = TerrainUnavailable "route_too_short_for_terrain_sampling"
  | spacing > routeMaximumSpacingM rules = TerrainUnavailable "route_too_long_for_detailed_terrain_sampling"
  | otherwise = TerrainSamples samples positions spacing
  where
    points = pathPoints path
    total = sum (zipWith distance points (drop 1 points))
    count = min (routeSampleLimit rules) (max (routeSampleMinimum rules) (ceiling (total / routeTargetSpacingM rules) + 1))
    spacing = total / fromIntegral (count - 1)
    (samples, positions) = samplePath count points

elevationRequest :: RouteRules -> [Point] -> Request
elevationRequest rules samples = Request "GET" (routeElevationUrl rules) [] params EmptyBody (routeHttpBytes rules) (routeElevationTimeout rules)
  where
    coordinate item = Text.pack (printf "%.6f,%.6f" (latitude item) (longitude item))
    locations = Text.intercalate "|" (map coordinate samples)
    params = [textParam "locations" locations]

terrain :: RouteRules -> Send -> Path -> IO (Terrain, [DetailedStep])
terrain rules send path = case planTerrain rules path of
  TerrainUnavailable reason -> pure (unavailable reason, pathSteps path)
  TerrainSamples samples positions spacing -> do
    let request = elevationRequest rules samples
        durationMicros = routeElevationTimeout rules * 1000000
    response <- timeout durationMicros (fetch "invalid_elevation_response" (send request))
    pure (terrainResult rules path samples positions spacing response)

terrainResult :: RouteRules -> Path -> [Point] -> [Double] -> Double -> Maybe (Result Elevations) -> (Terrain, [DetailedStep])
terrainResult rules path samples positions spacing response = case response of
  Nothing -> missing "elevation_timeout"
  Just (Left issue) -> missing (failureCode issue)
  Just (Right value) -> either missing finish (elevationValues rules samples spacing value)
  where
    missing reason = (unavailable reason, pathSteps path)
    finish values = (terrainSummary rules spacing values, steps)
      where steps = zipWith (stepGround rules positions (map elevationElevation values)) (pathSpans path) (pathSteps path)

terrainSummary :: RouteRules -> Double -> [Elevation] -> Terrain
terrainSummary rules spacing values = Terrain "estimated" "Google Maps Elevation API" Nothing (Just (length values)) (Just (rounded 1 spacing))
  (Just (rounded 1 resolution)) (Just (round gain)) (Just (round loss)) (Just (round net))
  (Just (routeElevationNoiseM rules)) (Just (routeTerrainLimitations rules))
  where
    elevations = map elevationElevation values
    resolution = foldl' max 0 (map (fromMaybe 0 . elevationResolution) values)
    (gain, loss) = gainLoss rules elevations
    net = lastOr 0 elevations - firstOr 0 elevations

elevationValues :: RouteRules -> [Point] -> Double -> Elevations -> Either Text [Elevation]
elevationValues rules samples spacing response
  | elevStatus response /= "OK" = Left statusError
  | length values /= length samples = Left "invalid_elevation_response"
  | not (and (zipWith (\expected actual -> distance expected (point (elevationLocation actual)) <= routeElevationLocationToleranceM rules) samples values)) = Left "elevation_locations_do_not_match_route"
  | any (isNothing . elevationResolution) values = Left "elevation_resolution_unknown"
  | any invalid values = Left "invalid_elevation_response"
  | resolution > max (routeElevationMinimumResolutionM rules) spacing = Left "elevation_resolution_too_coarse"
  | otherwise = Right values
  where
    values = fromMaybe [] (elevResults response)
    resolution = foldl' max 0 (map (fromMaybe 0 . elevationResolution) values)
    invalid value = elevationElevation value < routeElevationMinimumM rules || elevationElevation value > routeElevationMaximumM rules
      || maybe False (\metres -> metres < 0 || metres > routeElevationMaximumResolutionM rules) (elevationResolution value)
    statusError
      | elevStatus response `elem` routeElevationStatuses rules = "elevation_" <> Text.toLower (elevStatus response)
      | otherwise = "elevation_request_failed"

stepGround :: RouteRules -> [Double] -> [Double] -> (Double, Double) -> DetailedStep -> DetailedStep
stepGround rules positions elevations (begin, end) step = step {detailTerrain = Just ground}
  where
    samples = [(position, elevation) | (position, elevation) <- zip positions elevations, position >= begin, position <= end]
    covered = lastOr 0 (map fst samples) - firstOr 0 (map fst samples)
    values = map snd samples
    (gain, loss) = gainLoss rules values
    net = lastOr 0 values - firstOr 0 values
    ground
      | length values < routeSampleMinimum rules || covered < (end - begin) * routeStepCoverage rules =
        StepTerrain "unavailable" (Just "insufficient_samples_for_this_step") Nothing Nothing Nothing Nothing Nothing
      | otherwise = StepTerrain "estimated" Nothing (Just (length values)) (Just (round covered)) (Just (round gain)) (Just (round loss)) (Just (round net))

routeRequest :: RouteRules -> Place -> Place -> Result Request
routeRequest rules origin destination = do
  validateWaypoint rules origin
  validateWaypoint rules destination
  let resource = RouteRequest (placeWaypoint origin) (placeWaypoint destination) "WALK" (routeLanguage rules) "METRIC" False "HIGH_QUALITY" "GEO_JSON_LINESTRING"
      body = JsonBody (toJSON resource)
      fields = Text.encodeUtf8 (routeRouteFields rules)
  Right (Request "POST" (routeRouteUrl rules) [("X-Goog-FieldMask", fields)] [] body (routeHttpBytes rules) (routeHttpTimeout rules))

route :: RouteRules -> Send -> Bool -> Place -> Place -> IO (Result Value)
route rules send elevation origin destination = case routeRequest rules origin destination of
  Left issue -> pure (Left issue)
  Right request -> do
    response <- fetch "maps_invalid_route_response" (send request)
    either (pure . Left) (completeRoute rules send elevation) (response >>= normalize rules origin destination)

completeRoute :: RouteRules -> Send -> Bool -> (RouteResult, Path) -> IO (Result Value)
completeRoute rules _ False (value, _) = pure (finishRoute rules value (unavailable "not_enabled", resultSteps value))
completeRoute rules send True (value, path) = fmap (finishRoute rules value) (terrain rules send path)

finishRoute :: RouteRules -> RouteResult -> (Terrain, [DetailedStep]) -> Result Value
finishRoute rules value (ground, steps) = toJSON <$> routeBudget rules result
  where result = value {resultTerrain = Just ground, resultSteps = steps}

validateWaypoint :: RouteRules -> Place -> Result ()
validateWaypoint rules place = validPoint (placePoint place) >> validId
  where
    ident = placeId place
    validId
      | Text.length ident > routePlaceIdChars rules || Text.any (< ' ') ident = Left (failure "maps_invalid_route_place")
      | otherwise = Right ()
