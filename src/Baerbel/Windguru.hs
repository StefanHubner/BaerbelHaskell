{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Windguru (windguru, forecast, Numeric(..)) where

import Baerbel.TimeFormat (isoUtc, isoZoned)
import Data.Either (rights)
import Data.Char (isDigit)
import Baerbel.Cache (memoize)
import Baerbel.Effects (continue, mapResult, within, monotonicSeconds)
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Body(..), Request(..), Send, requestJSON)
import Baerbel.JSON (argumentOptions, jsonOptions, decodeValue)
import Baerbel.Provider
import Baerbel.ProviderConfig (WindguruRules(..))
import Control.Applicative (empty, (<|>))
import System.Timeout (timeout)
import Data.Aeson hiding (Result, pairs)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.List (nub, nubBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isNothing, mapMaybe, fromMaybe, isJust, catMaybes, listToMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

type LocalZone = Text -> UTCTime -> Result ZonedTime

newtype Numeric = Numeric {numericValue :: Double} deriving (Eq, Show)
instance FromJSON Numeric where
  parseJSON (Number value) = pure (Numeric (toRealFloat value))
  parseJSON (String value) | not (Text.null value) && Text.length value <= 9 && Text.all isDigit value = maybe empty (pure . Numeric) (readMaybe (Text.unpack value))
  parseJSON _ = empty
instance ToJSON Numeric where toJSON = toJSON . numericValue
newtype Flag = Flag {flagValue :: Bool} deriving (Eq, Show)
instance FromJSON Flag where
  parseJSON (Bool value) = pure (Flag value)
  parseJSON (Number 0) = pure (Flag False)
  parseJSON (Number 1) = pure (Flag True)
  parseJSON (String "0") = pure (Flag False)
  parseJSON (String "1") = pure (Flag True)
  parseJSON _ = empty

data RawSpot = RawSpot
  { rawIdSpot :: Numeric, rawSpotname :: Maybe Text, rawCountry :: Maybe Text
  , rawLat :: Double, rawLon :: Double, rawTzid :: Maybe Text, rawTz :: Maybe Text
  , rawModels :: [Numeric], rawSunrise :: Maybe Text, rawSunset :: Maybe Text, rawTide :: Maybe Value } deriving (Generic)
instance FromJSON RawSpot where parseJSON = genericParseJSON (jsonOptions 3)
newtype Metadata = Metadata {metaSpots :: Map Text Value} deriving (Generic)
instance FromJSON Metadata where parseJSON = genericParseJSON (jsonOptions 4)
data SearchRow = SearchRow {rowIdSpot :: Numeric, rowSpotname :: Maybe Text, rowCountry :: Maybe Text} deriving (Generic)
instance FromJSON SearchRow where parseJSON = genericParseJSON (jsonOptions 3)
data SearchResponse = SearchResponse {searchCount :: Int, searchSpots :: [SearchRow]} deriving (Generic)
instance FromJSON SearchResponse where parseJSON = genericParseJSON (jsonOptions 6)
data RawModel = RawModel
  { modelModelName :: Maybe Text, modelWave :: Maybe Flag, modelVirtual :: Maybe Flag, modelResolution :: Maybe Double } deriving (Generic)
instance FromJSON RawModel where
  parseJSON (Object value) = pure (RawModel (optionalField "model_name" value) (optionalField "wave" value) (optionalField "virtual" value) (optionalField "resolution" value))
  parseJSON _ = pure (RawModel Nothing Nothing Nothing Nothing)

data ForecastData = ForecastData
  { forecastInitstamp :: Double, forecastHours :: [Double], forecastModelName :: Text
  , forecastFields :: Map Text Value } deriving (Show)
instance FromJSON ForecastData where parseJSON = withObject "Forecast" parseForecast

parseForecast :: Object -> Parser ForecastData
parseForecast payload = do
  stamp <- payload .: "initstamp"
  hours <- payload .: "hours"
  name <- payload .:? "model_name" .!= ""
  longName <- payload .:? "model_longname" .!= ""
  let title
        | Text.null name = longName
        | otherwise = name
      arrays = Map.fromList [(Key.toText key, value) | (key, value) <- KeyMap.toList payload]
  pure (ForecastData stamp hours title arrays)

newtype Reading = Reading {readingValue :: Maybe Double}
instance FromJSON Reading where
  parseJSON Null = pure (Reading Nothing)
  parseJSON (String "") = pure (Reading Nothing)
  parseJSON value = Reading . Just <$> parseJSON value

data RawForecast = RawForecast {rawForecastIdSpot :: Numeric, rawForecastIdModel :: Numeric, rawForecastFcst :: Maybe Value} deriving (Generic)
instance FromJSON RawForecast where parseJSON = genericParseJSON (jsonOptions 11)

data SearchArgs = SearchArgs {argQuery :: Text, argCountry :: Maybe Text, argLimit :: Int} deriving (Generic)
instance FromJSON SearchArgs where parseJSON = genericParseJSON (argumentOptions 3)
newtype SpotArgs = SpotArgs {argSpotId :: Int} deriving (Generic)
instance FromJSON SpotArgs where parseJSON = genericParseJSON (argumentOptions 3)
data ForecastArgs = ForecastArgs {argSpotIdForecast :: Int, argModelIdForecast :: Int, argStart :: Maybe Text, argHours :: Int} deriving (Generic)
instance FromJSON ForecastArgs where
  parseJSON = genericParseJSON (renamedOptions 3 [("argSpotIdForecast", "spot_id"), ("argModelIdForecast", "model_id")]) {rejectUnknownFields = True, allowOmittedFields = False}
data SetArgs = SetArgs {setStart :: Maybe Text, setHours :: Int} deriving (Generic)
instance FromJSON SetArgs where parseJSON = genericParseJSON (argumentOptions 3)

data Spot = Spot
  { spotSpotId :: Int, spotName :: Text, spotCountry :: Maybe Text, spotLatitude :: Double
  , spotLongitude :: Double, spotTimezone :: Maybe Text, spotSourceUrl :: Text } deriving (Eq, Show, Generic)
instance ToJSON Spot where toJSON = genericToJSON (jsonOptions 4)
data Candidate = Candidate
  { candidateSpotId :: Int, candidateName :: Text, candidateCountry :: Maybe Text, candidateSourceUrl :: Text } deriving (Generic)
instance ToJSON Candidate where toJSON = genericToJSON (jsonOptions 9)
data SearchResult = SearchResult
  { resultStatus :: Text, resultSource :: Text, resultQuery :: Text, resultCountryFilter :: Maybe Text
  , resultAvailableCountries :: [Text], resultAvailableCountriesTruncated :: Bool, resultUnknownCountryResults :: Int
  , resultCandidates :: [Candidate], resultResultCount :: Int, resultMatchesInProviderResults :: Int
  , resultProviderResultsTruncated :: Bool, resultHasMore :: Bool, resultRetrievedAt :: Text
  , resultSearchScope :: Text, resultUntrustedProviderContent :: Bool } deriving (Generic)
instance ToJSON SearchResult where toJSON = genericToJSON (jsonOptions 6)
data Model = Model {outModelId :: Int, outName :: Text, outKind :: Text, outVirtual :: Maybe Bool, outResolutionKm :: Maybe Double} deriving (Generic)
instance ToJSON Model where toJSON = genericToJSON (jsonOptions 3)
data Sun = Sun
  { sunDate :: Maybe Text, sunDateVerified :: Bool, sunTimezone :: Maybe Text, sunSource :: Text
  , sunSunriseLocal :: Maybe Text, sunSunsetLocal :: Maybe Text } deriving (Generic)
instance ToJSON Sun where toJSON = genericToJSON (jsonOptions 3)
data Tide = Tide {tideAvailable :: Bool, tideHarmonicMetadataPresent :: Maybe Bool, tideReason :: Text} deriving (Generic)
instance ToJSON Tide where toJSON = genericToJSON ((jsonOptions 4) {omitNothingFields = True})
data SpotResult = SpotResult
  { spotResultStatus :: Text, spotResultSource :: Text, spotResultSpot :: Spot, spotResultRetrievedAt :: Text
  , spotResultModels :: [Model], spotResultModelCatalogError :: Maybe Text, spotResultSun :: Sun
  , spotResultTidePredictions :: Tide, spotResultUntrustedProviderContent :: Bool } deriving (Generic)
instance ToJSON SpotResult where toJSON = genericToJSON (jsonOptions 10)
data TimePoint = TimePoint {timeTimeUtc :: Text, timeTimeLocal :: Maybe Text} deriving (Generic)
instance ToJSON TimePoint where toJSON = genericToJSON (jsonOptions 4)
data ForecastPoint = ForecastPoint {pointTime :: TimePoint, pointNumbers :: Map Text (Maybe Double)}
instance ToJSON ForecastPoint where toJSON value = combine [toJSON (pointTime value), toJSON (pointNumbers value)]
data Forecast = Forecast
  { outStatus :: Text, outSource :: Text, outSpot :: Spot, outModelIdForecast :: Int, outModelName :: Text
  , outRetrievedAt :: Text, outInitializedAt :: Text, outInitializationAgeHours :: Double, outOlderThan24Hours :: Bool
  , outRequestedStart :: Text, outRequestedEndExclusive :: Text, outAvailableStart :: Text, outAvailableEnd :: Text
  , outPoints :: [ForecastPoint], outPointCount :: Int, outTruncated :: Bool, outAvailability :: Text
  , outUnits :: Map Text Text, outDirectionConventions :: Map Text Text, outSun :: Sun
  , outTidePredictions :: Tide, outUntrustedProviderContent :: Bool } deriving (Generic)
instance ToJSON Forecast where
  toJSON = genericToJSON forecastOptions

forecastOptions :: Options
forecastOptions = renamedOptions 3 [("outModelIdForecast", "model_id"), ("outOlderThan24Hours", "older_than_24_hours")]

spot :: WindguruRules -> RawSpot -> Spot
spot rules value = Spot ident (clean (windguruNameChars rules) (fromMaybe "" (rawSpotname value))) (clean (windguruCountryChars rules) <$> rawCountry value)
  (rawLat value) (rawLon value) selected url
  where
    ident = round (numericValue (rawIdSpot value))
    selected = rawTzid value <|> rawTz value
    url = windguruBaseUrl rules <> Text.pack (show ident)

sun :: WindguruRules -> RawSpot -> Sun
sun rules value = Sun Nothing False (spotTimezone (spot rules value)) (windguruSunSource rules) (clockValue (rawSunrise value)) (clockValue (rawSunset value))

clockValue :: Maybe Text -> Maybe Text
clockValue value = value >>= valid
  where
    valid text = case parseTimeM True defaultTimeLocale "%H:%M" (Text.unpack text) :: Maybe TimeOfDay of
      Just _ | Text.length text == 5 -> Just text
      _ -> Nothing

iso :: UTCTime -> Text
iso = isoUtc

data Window = Window {windowStart :: Maybe UTCTime, windowHours :: Int}
data ForecastQuery = ForecastQuery {querySpot :: Int, queryModel :: Int, queryWindow :: Window}

parseStart :: WindguruRules -> Text -> Result UTCTime
parseStart rules value
  | Text.length value > windguruStartChars rules = Left (failure "windguru_invalid_arguments")
  | otherwise = maybe (Left (failure "windguru_invalid_arguments")) (Right . zonedTimeToUTC) parsed
  where
    normalized
      | "Z" `Text.isSuffixOf` value = Text.dropEnd 1 value <> "+00:00"
      | otherwise = value
    parsed = parseTimeM True defaultTimeLocale "%FT%T%Q%Ez" (Text.unpack normalized)

parseWindow :: WindguruRules -> Int -> Maybe Text -> Result Window
parseWindow rules hours supplied
  | hours < 1 || hours > windguruMaximumPoints rules = Left (failure "windguru_invalid_arguments")
  | otherwise = do
    start <- traverse (parseStart rules) supplied
    Right (Window start hours)

forecast :: WindguruRules -> LocalZone -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> RawForecast -> Result Forecast
forecast rules local now raw model start hours response = do
  dataValue <- case rawForecastFcst response of
    Just value@(Object _) -> decodeValue "windguru_invalid_response" value
    _ -> Left (failure "windguru_forecast_unavailable")
  let selectedFields = Map.filterWithKey (\key value -> key `elem` fieldNames && value /= Null) (forecastFields dataValue)
      fieldNames = map (\(name, _, _, _) -> name) (windguruFields rules)
  arrays <- traverse (decodeValue "windguru_invalid_response") selectedFields
  normalizeForecast rules local now raw model start hours response dataValue arrays

validateForecastData :: WindguruRules -> UTCTime -> Int -> Int -> RawForecast -> ForecastData -> Map Text [Value] -> Result ()
validateForecastData rules now expectedSpot model response dataValue arrays
  | not (validNumericId (windguruSpotLimit rules) (rawForecastIdSpot response)) || not (validNumericId (windguruModelLimit rules) (rawForecastIdModel response)) = invalid
  | stamp < windguruMinimumTimestamp rules || stamp > windguruMaximumTimestamp rules || any (\hour -> hour < 0 || hour > windguruMaximumSourceHours rules) sourceHours = invalid
  | not (any (`Map.member` arrays) (windguruRequiredFields rules)) = Left (failure "windguru_forecast_unavailable")
  | round (numericValue (rawForecastIdSpot response)) /= expectedSpot || round (numericValue (rawForecastIdModel response)) /= model = Left (failure "windguru_invalid_response")
  | null sourceHours || length sourceHours > windguruMaximumSourcePoints rules || not (and (zipWith (<) sourceHours (drop 1 sourceHours))) = Left (failure "windguru_invalid_response")
  | any ((/= length sourceHours) . length) (Map.elems arrays) = Left (failure "windguru_invalid_response")
  | initialized > addUTCTime (realToFrac (windguruFutureToleranceSeconds rules)) now = Left (failure "windguru_invalid_response")
  | otherwise = Right ()
  where
    invalid = Left (failure "windguru_invalid_response")
    stamp = forecastInitstamp dataValue
    initialized = posixSecondsToUTCTime (realToFrac stamp)
    sourceHours = forecastHours dataValue

normalizeForecast :: WindguruRules -> LocalZone -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> RawForecast -> ForecastData -> Map Text [Value] -> Result Forecast
normalizeForecast rules local now raw model start hours response dataValue arrays = do
  validateForecastData rules now (spotSpotId identity) model response dataValue arrays
  points <- traverse (forecastPoint rules local (spotTimezone identity) initialized arrays) selected
  let age :: Double
      age = max 0 (realToFrac (diffUTCTime now initialized) / 3600)
      availableFirst = at (firstOr 0 sourceHours)
      availableLast = at (foldl (\_ value -> value) 0 sourceHours)
      tide = Tide False Nothing (windguruTideReason rules)
  Right (Forecast "ok" "windguru_public_forecast" identity model (clean (windguruModelChars rules) (forecastModelName dataValue)) (iso now) (iso initialized)
    (fromIntegral (round (age * 100) :: Int) / 100) (age > 24) (iso start) (iso finish) (iso availableFirst) (iso availableLast) points (length points) truncated
    (availability points) units conventions (sun rules raw) tide True)
  where
    identity = spot rules raw
    initialized = posixSecondsToUTCTime (realToFrac (forecastInitstamp dataValue))
    sourceHours = forecastHours dataValue
    at hour = addUTCTime (realToFrac (hour * 3600)) initialized
    finish = addUTCTime (fromIntegral hours * 3600) start
    available = filter (\(_, hour) -> at hour >= start && at hour < finish) (zip [0..] sourceHours)
    selected = take (windguruMaximumPoints rules) available
    truncated = length available > windguruMaximumPoints rules
    units = windguruUnits rules
    conventions = Map.fromList [("wind", windguruWindDirectionConvention rules), ("waves", windguruWaveDirectionConvention rules)]

forecastPoint :: WindguruRules -> LocalZone -> Maybe Text -> UTCTime -> Map Text [Value] -> (Int, Double) -> Result ForecastPoint
forecastPoint rules local zone initialized arrays (index, hour) = do
  values <- traverse (fieldValue index) (windguruFields rules)
  let numeric = Map.fromListWith prefer (catMaybes values)
      instant = addUTCTime (realToFrac (hour * 3600)) initialized
      localTime = zone >>= either (const Nothing) (Just . isoZoned) . (`local` instant)
      timestamp = TimePoint (iso instant) localTime
  Right (ForecastPoint timestamp numeric)
  where
    prefer later Nothing = later
    prefer _ earlier = earlier
    fieldValue offset (source, target, lower, upper) = case Map.lookup source arrays of
      Nothing -> Right Nothing
      Just values -> Just . (target,) <$> readingNumber lower upper (firstOr Null (drop offset values))

availability :: [ForecastPoint] -> Text
availability [] = "no_points_in_requested_window"
availability _ = "available"

readingNumber :: Double -> Double -> Value -> Result (Maybe Double)
readingNumber lower upper raw = do
  reading <- decodeValue "windguru_invalid_response" raw
  case readingValue reading of
    Nothing -> Right Nothing
    Just value | value >= lower && value <= upper -> Right (Just value)
    _ -> Left (failure "windguru_invalid_response")

data Received a = Received {receivedValue :: a, receivedAt :: UTCTime}
type TimedSend = Request -> IO (Result (Received Value))

decodeReceived :: FromJSON a => Text -> Received Value -> Result (Received a)
decodeReceived code value = (`Received` receivedAt value) <$> decodeValue code (receivedValue value)

request :: WindguruRules -> Text -> [(Text, Text)] -> Request
request rules query params = Request "GET" (windguruApiUrl rules) headers queryParams EmptyBody (windguruHttpBytes rules) (windguruHttpTimeout rules)
  where
    queryParams = textParam "q" query : map (\(key, value) -> textParam (Text.encodeUtf8 key) value) params
    suffix = case query of
      "forecast_set" -> ("?set=" <>) <$> lookup "set" params
      _ -> lookup "id_spot" params
    referer = windguruBaseUrl rules <> fromMaybe "" suffix
    headers = [("Accept", "application/json"), ("Referer", Text.encodeUtf8 referer), ("User-Agent", Text.encodeUtf8 (windguruUserAgent rules))]

getSpot :: WindguruRules -> Int -> Received Metadata -> Result (Received RawSpot)
getSpot rules ident received = do
  let key = Text.pack (show ident)
  payload <- maybe (Left (failure "windguru_spot_not_found")) Right (Map.lookup key (metaSpots (receivedValue received)))
  raw <- decodeValue "windguru_invalid_response" payload
  value <- validateSpot rules ident raw
  Right (Received value (receivedAt received))

search :: WindguruRules -> SearchArgs -> Received SearchResponse -> Result SearchResult
search rules args received
  | searchCount value < length rows || not (all (validNumericId (windguruSpotLimit rules) . rowIdSpot) rows) = Left (failure "windguru_invalid_response")
  | otherwise = Right (SearchResult "ok" "windguru_public_search" (argQuery args) (argCountry args) (take (windguruCountryLimit rules) countries)
    (length countries > windguruCountryLimit rules) unknown candidates (length candidates) (length matched) providerCut
    (length matched > argLimit args || providerCut) (iso (receivedAt received)) (windguruSearchScope rules) True)
  where
    value = receivedValue received
    rows = map normalizeRow (searchSpots value)
    countries = nubBy (\first second -> Text.toCaseFold first == Text.toCaseFold second) (filter (not . Text.null) (mapMaybe rowCountry rows))
    unknown = length (filter (maybe True Text.null . rowCountry) rows)
    normalizeRow item = item {rowCountry = clean (windguruCountryChars rules) <$> rowCountry item}
    matches row = maybe True (\country -> fmap Text.toCaseFold (rowCountry row) == Just (Text.toCaseFold country)) (argCountry args)
    matched = filter matches rows
    candidates = map (searchCandidate rules) (take (argLimit args) matched)
    providerCut = searchCount value > length rows

searchCandidate :: WindguruRules -> SearchRow -> Candidate
searchCandidate rules row = Candidate ident name (rowCountry row) url
  where
    ident = round (numericValue (rowIdSpot row))
    name = clean (windguruNameChars rules) (fromMaybe "" (rowSpotname row))
    url = windguruBaseUrl rules <> Text.pack (show ident)

metadata :: WindguruRules -> LocalZone -> Result (Received (Map Text RawModel)) -> Received RawSpot -> SpotResult
metadata rules local catalog received = SpotResult "ok" "windguru_public_spot" (spot rules value) (iso now) models problem (sun rules value) tide True
  where
    now = receivedAt received
    value = withZone local now (receivedValue received)
    models = map (modelInfo rules catalog . round . numericValue) (rawModels value)
    problem = either (Just . failureCode) (const Nothing) catalog
    tide = Tide False (Just (fromMaybe False (rawTide value >>= parseMaybe (withObject "tide" (pure . not . KeyMap.null))))) (windguruMetadataTideReason rules)

modelInfo :: WindguruRules -> Result (Received (Map Text RawModel)) -> Int -> Model
modelInfo rules catalog ident = Model ident name kind virtual resolution
  where
    item = either (const Nothing) (Map.lookup (Text.pack (show ident)) . receivedValue) catalog
    name = clean (windguruModelChars rules) (fromMaybe "" (item >>= modelModelName))
    kind = case flagValue <$> (item >>= modelWave) of
      Nothing -> "unknown"
      Just True -> "wave"
      Just False -> "weather"
    virtual = flagValue <$> (item >>= modelVirtual)
    resolution = case item >>= modelResolution of
      Just value | value >= 0 && value <= windguruResolutionLimit rules -> Just value
      _ -> Nothing

forecastRequest :: WindguruRules -> Int -> Int -> Request
forecastRequest rules spotId modelId = request rules "forecast" params
  where params = [("id_spot", Text.pack (show spotId)), ("id_model", Text.pack (show modelId))]

availableModel :: Int -> RawSpot -> Result RawSpot
availableModel model value
  | model `elem` map (round . numericValue) (rawModels value) = Right value
  | otherwise = Left (failure "windguru_model_unavailable_for_spot")

getForecast :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> ForecastQuery -> IO (Result Value)
getForecast rules send clock local query = do
  let spotRequest = request rules "forecast_spot" [("id_spot", Text.pack (show (querySpot query)))]
  response <- mapResult (decodeReceived "windguru_invalid_response") (send spotRequest)
  let spotResponse = response >>= getSpot rules (querySpot query)
      selected = spotResponse >>= availableModel (queryModel query) . receivedValue
  continue retrieve selected
  where
    retrieve raw = do
      let wire = forecastRequest rules (querySpot query) (queryModel query)
      response <- mapResult (decodeReceived "windguru_invalid_response") (send wire)
      now <- clock
      let window = queryWindow query
          start = fromMaybe now (windowStart window)
          normalized = response >>= normalizeReceived rules local now raw (queryModel query) start (windowHours window)
      pure (fmap toJSON normalized)

newtype SetModel
  = SetModel {selectedIdModel :: Numeric}
  deriving (Generic)
instance FromJSON SetModel where parseJSON = genericParseJSON (jsonOptions 8)
data SetTab = SetTab
  { tabIdSpot :: Numeric, tabIdModelArr :: [SetModel], tabProOnlyMessage :: Maybe Text
  , tabIdModel :: Maybe Numeric, tabIdModelWave :: Maybe Numeric, tabBlend :: Maybe Flag } deriving (Generic)
instance FromJSON SetTab where parseJSON = genericParseJSON (jsonOptions 3)
data ForecastSet = ForecastSet
  { setDataSet :: Maybe Text, setDataSpots :: Map Text RawSpot, setDataTabs :: [SetTab] } deriving (Generic)
data SetFrame = SetFrame {frameSet :: Maybe Value, frameSpots :: Map Text Value, frameTabs :: [Value]} deriving (Generic)
instance FromJSON SetFrame where parseJSON = genericParseJSON (jsonOptions 5)
data SetIdentity = SetIdentity {identitySetId :: Int, identityName :: Text, identitySourceUrl :: Text} deriving (Generic)
instance ToJSON SetIdentity where toJSON = genericToJSON (jsonOptions 8)
data SetExtra = SetExtra
  { setResultSet :: SetIdentity, setResultSelection :: Selection
  , setResultSetRetrievedAt :: Text, setResultSuccessfulForecasts :: Int
  , setResultFailedForecasts :: Int, setResultPartial :: Bool, setResultDeadlineReached :: Bool } deriving (Generic)
instance ToJSON SetExtra where toJSON = genericToJSON (jsonOptions 9)
data Table = Table
  { tableTableIndex :: Int, tableSpotId :: Int, tableModelIds :: [Int], tableModelIdsTruncated :: Bool
  , tablePrimaryModelId :: Maybe Int, tableWaveModelId :: Maybe Int, tableBlendRequested :: Bool
  , tableAccessRestricted :: Bool } deriving (Generic)
instance ToJSON Table where toJSON = genericToJSON (jsonOptions 5)
data Selection = Selection
  { selectionTables :: [Table], selectionTableCount :: Int, selectionTablesTruncated :: Bool
  , selectionSelectedPairCount :: Int, selectionRequestedPairCount :: Int, selectionPairsTruncated :: Bool
  , selectionTableTransformationsApplied :: Bool, selectionModelInterpretation :: Text } deriving (Generic)
instance ToJSON Selection where toJSON = genericToJSON (jsonOptions 9)
data Columns = Columns
  { columnStatus :: Text, columnSpot :: Spot, columnModelId :: Int, columnModelName :: Text
  , columnRetrievedAt :: Text, columnInitializedAt :: Text, columnInitializationAgeHours :: Double
  , columnOlderThan24Hours :: Bool, columnAvailableStart :: Text, columnAvailableEnd :: Text
  , columnAvailability :: Text, columnSun :: Sun, columnColumns :: Map Text [Value]
  , columnAvailablePointCount :: Int, columnReturnedPointCount :: Int
  , columnProviderTruncated :: Bool, columnTruncated :: Bool, columnOmittedColumns :: Maybe [Text] } deriving (Generic)
instance ToJSON Columns where toJSON = genericToJSON ((renamedOptions 6 [("columnOlderThan24Hours", "older_than_24_hours")]) {omitNothingFields = True})
data Comparison = Comparison
  { compareStatus :: Text, compareSource :: Text, compareUnits :: Maybe (Map Text Text)
  , compareDirectionConventions :: Maybe (Map Text Text), compareTidePredictions :: Maybe Tide
  , compareRequestedStart :: Maybe Text, compareRequestedEndExclusive :: Maybe Text
  , compareForecasts :: [Value], compareInputForecastCount :: Int, compareForecastCount :: Int
  , compareOmittedForecastCount :: Int, compareFailedForecastCount :: Int, compareTruncated :: Bool
  , compareDetailHint :: Text, compareUntrustedProviderContent :: Bool } deriving (Generic)
instance ToJSON Comparison where toJSON = genericToJSON ((jsonOptions 7) {omitNothingFields = True})

data FailedForecast = FailedForecast
  { failedStatus :: Text, failedError :: Text, failedModelId :: Int, failedSpotId :: Int, failedSpot :: Spot } deriving (Generic)
instance ToJSON FailedForecast where toJSON = genericToJSON (jsonOptions 6)

type SelectedPair = (RawSpot, Int, Bool)

setEntries :: WindguruRules -> ForecastSet -> ([SelectedPair], Selection)
setEntries rules value = (selected, selection)
  where
    tabs = setDataTabs value
    pairs = [(raw, round (numericValue (selectedIdModel model)), maybe False (not . Text.null) (tabProOnlyMessage tab))
      | tab <- tabs, let key = Text.pack (show (round (numericValue (tabIdSpot tab)) :: Int))
      , Just raw <- [Map.lookup key (setDataSpots value)], model <- tabIdModelArr tab]
    same (spotA, modelA, _) (spotB, modelB, _) = rawIdSpot spotA == rawIdSpot spotB && modelA == modelB
    unique = nubBy same pairs
    selected = take (windguruMaximumSetPairs rules) unique
    tables = map table (take (windguruMaximumSetPairs rules) (zip [0..] tabs))
    selection = Selection tables (length tabs) (length tabs > length tables) (length unique) (length selected)
      (length unique > length selected) False (windguruModelInterpretation rules)
    table (index, tab) = Table index (round (numericValue (tabIdSpot tab))) limited (length models > length limited)
      (round . numericValue <$> tabIdModel tab) (round . numericValue <$> tabIdModelWave tab)
      (maybe False flagValue (tabBlend tab)) (maybe False (not . Text.null) (tabProOnlyMessage tab))
      where
        models = nub (map (round . numericValue . selectedIdModel) (tabIdModelArr tab))
        limited = take (windguruMaximumSetPairs rules) models

columnar :: WindguruRules -> Int -> Forecast -> Columns
columnar rules count value = Columns "ok" (outSpot value) (outModelIdForecast value) (outModelName value)
  (outRetrievedAt value) (outInitializedAt value) (outInitializationAgeHours value) (outOlderThan24Hours value)
  (outAvailableStart value) (outAvailableEnd value) (outAvailability value) (outSun value) columns available returned
  (outTruncated value) (outTruncated value || returned < available) omittedColumns
  where
    points = outPoints value
    available = length points
    returned = min available count
    selected = take count points
    localTimes = not (null points) && all (isJust . timeTimeLocal . pointTime) points
    timeKey
      | localTimes = "time_local"
      | otherwise = "time_utc"
    timestamp pointValue
      | localTimes = toJSON (timeTimeLocal (pointTime pointValue))
      | otherwise = toJSON (timeTimeUtc (pointTime pointValue))
    present key = any (Map.member key . pointNumbers) points
    keys = filter present (windguruComparisonColumns rules)
    numeric key pointValue = toJSON (fromMaybe Nothing (Map.lookup key (pointNumbers pointValue)))
    columns = Map.fromList ((timeKey, map timestamp selected) : [(key, map (numeric key) selected) | key <- keys])
    omittedColumns = case filter present (windguruOmittedColumns rules) of
      [] -> Nothing
      names -> Just names

compactForecasts :: WindguruRules -> [Either FailedForecast Forecast] -> Result Comparison
compactForecasts rules results
  | size complete <= budget = Right complete
  | size emptyRows > budget = Left (failure "windguru_comparison_budget_too_small")
  | otherwise = Right (searchBudget 0 (windguruMaximumPoints rules) emptyRows)
  where
    budget = windguruComparisonBytes rules
    entries = take (windguruMaximumSetPairs rules) results
    successful = rights entries
    first = listToMaybe successful
    render cap = Comparison "ok" "windguru_public_forecast_comparison" (outUnits <$> first)
      (outDirectionConventions <$> first) (outTidePredictions <$> first) (outRequestedStart <$> first)
      (outRequestedEndExclusive <$> first) (map (either toJSON (toJSON . columnar rules cap)) entries)
      (length results) (length entries) (length results - length entries) (length entries - length successful)
      (length results > length entries || any (\value -> outTruncated value || length (outPoints value) > cap) successful)
      (windguruDetailHint rules) True
    complete = render (windguruMaximumPoints rules)
    emptyRows = render 0
    size = referenceByteSize
    searchBudget low high previous
      | low >= high = previous
      | size candidate <= budget = searchBudget middle high candidate
      | otherwise = searchBudget low (middle - 1) previous
      where
        middle = (low + high + 1) `div` 2
        candidate = render middle

normalizeReceived :: WindguruRules -> LocalZone -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> Received RawForecast -> Result Forecast
normalizeReceived rules local now raw model start hours received = do
  value <- forecast rules local now (withZone local now raw) model start hours (receivedValue received)
  Right value {outRetrievedAt = iso (receivedAt received)}

newtype Deadline = Deadline Double

newDeadline :: Int -> IO Deadline
newDeadline seconds = do
  now <- monotonicSeconds
  let end = now + fromIntegral seconds
  pure (Deadline end)

beforeDeadline :: Deadline -> IO a -> IO (Maybe a)
beforeDeadline (Deadline end) action = do
  now <- monotonicSeconds
  let remaining = max 0 (end - now)
      micros = floor (remaining * 1000000)
  timeout micros action

parseSetId :: WindguruRules -> Text -> Result Int
parseSetId rules value = case readMaybe (Text.unpack (Text.strip value)) of
  Just ident | ident >= 1 && ident <= windguruSpotLimit rules -> Right ident
  _ -> Left (failure "windguru_invalid_forecast_set_id")

forecastSet :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> IO (Result Text) -> Window -> IO (Result Value)
forecastSet rules send clock local readSet window = do
  deadline <- newDeadline (windguruHttpTimeout rules)
  saved <- beforeDeadline deadline readSet
  let selected = fromMaybe (Left (failure "windguru_timeout")) saved >>= parseSetId rules
  continue (loadSet rules send clock local deadline window) selected

loadSet :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> Deadline -> Window -> Int -> IO (Result Value)
loadSet rules send clock local deadline window ident = do
  let wire = request rules "forecast_set" [("set", Text.pack (show ident))]
  response <- beforeDeadline deadline (mapResult (decodeSet rules) (send wire))
  let decoded = fromMaybe (Left (failure "windguru_timeout")) response
  continue retrieve decoded
  where
    retrieve received = do
      now <- clock
      let prepared = validateSet rules (receivedValue received)
          start = fromMaybe now (windowStart window)
      continue (setMembers rules send clock local deadline ident (receivedAt received) start (windowHours window)) prepared

setMembers :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> Deadline -> Int -> UTCTime -> UTCTime -> Int -> ForecastSet -> IO (Result Value)
setMembers rules send clock local deadline ident retrieved start hours value = do
  let (selected, selection) = setEntries rules value
  results <- boundedMap (windguruSetConcurrency rules) (fetchMember rules send clock local deadline start hours) selected
  let ended = any fst results
      values = map snd results
  pure (finishSet rules ident value retrieved start hours selection ended values)

planMember :: WindguruRules -> SelectedPair -> Either FailedForecast Request
planMember rules (raw, model, restricted)
  | restricted = Left (failedForecast rules raw model "windguru_forecast_requires_login_or_pro")
  | otherwise = either (Left . failedForecast rules raw model . failureCode) (const (Right wire)) (availableModel model raw)
  where wire = forecastRequest rules (round (numericValue (rawIdSpot raw))) model

fetchMember :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> Deadline -> UTCTime -> Int -> SelectedPair -> IO (Bool, Either FailedForecast Forecast)
fetchMember rules send clock local deadline start hours selected@(raw, model, _) = case planMember rules selected of
  Left issue -> pure (False, Left issue)
  Right wire -> do
    response <- beforeDeadline deadline (mapResult (decodeReceived "windguru_invalid_response") (send wire))
    now <- clock
    pure (memberResult rules local now raw model start hours response)

memberResult :: WindguruRules -> LocalZone -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> Maybe (Result (Received RawForecast)) -> (Bool, Either FailedForecast Forecast)
memberResult rules local now raw model start hours response = (isNothing response, either failed Right normalized)
  where
    decoded = fromMaybe (Left (failure "windguru_timeout")) response
    normalized = decoded >>= normalizeReceived rules local now raw model start hours
    failed = Left . failedForecast rules raw model . failureCode

failedForecast :: WindguruRules -> RawSpot -> Int -> Text -> FailedForecast
failedForecast rules raw model code = FailedForecast "error" code model (spotSpotId identity) identity
  where identity = spot rules raw

finishSet :: WindguruRules -> Int -> ForecastSet -> UTCTime -> UTCTime -> Int -> Selection -> Bool -> [Either FailedForecast Forecast] -> Result Value
finishSet rules ident value now start hours selection ended results = do
  compact <- compactForecasts rules results
  let failures = length [() | Left _ <- results]
      successes = length results - failures
      cut = compareTruncated compact || selectionPairsTruncated selection || selectionTablesTruncated selection
      identity = SetIdentity ident (clean (windguruNameChars rules) (fromMaybe "" (setDataSet value))) (windguruBaseUrl rules <> "?set=" <> Text.pack (show ident))
      finish = addUTCTime (fromIntegral hours * 3600) start
      updated = compact {compareSource = "windguru_public_forecast_set", compareRequestedStart = Just (iso start), compareRequestedEndExclusive = Just (iso finish), compareTruncated = cut}
      extra = SetExtra identity selection (iso now) successes failures (failures > 0 || cut) ended
      output = combine [toJSON updated, toJSON extra]
  setBudget rules output

setBudget :: WindguruRules -> Value -> Result Value
setBudget rules output
  | referenceByteSize output > windguruResultBytes rules = Left (failure "windguru_forecast_set_result_too_large")
  | otherwise = Right output

windguru :: WindguruRules -> Send -> IO UTCTime -> LocalZone -> IO (Result Text) -> IO Execute
windguru rules send clock local readSet = do
  cached <- memoize (windguruCacheEntries rules) (windguruCacheSeconds rules) key retrieve
  pure (execute rules cached clock local readSet)
  where
    key requestValue = (requestUrl requestValue, requestQuery requestValue)
    retrieve requestValue = do
      response <- requestJSON send requestValue apiResult
      continue timestamp response
    timestamp value = Right . Received value <$> clock

apiResult :: Value -> Result Value
apiResult value@(Object envelope)
  | KeyMap.lookup "return" envelope == Just (String "error") || maybe False truthy (KeyMap.lookup "error" envelope) = Left (failure "windguru_api_error")
  | otherwise = Right value
apiResult _ = Left (failure "windguru_invalid_response")

validSearch :: WindguruRules -> SearchArgs -> Result SearchArgs
validSearch rules args
  | not (validText (windguruQueryChars rules) (argQuery args)) = invalid
  | not (maybe True (validText (windguruCountryChars rules)) (argCountry args)) = invalid
  | argLimit args < 1 || argLimit args > windguruSearchLimit rules = invalid
  | otherwise = Right args {argQuery = Text.strip (argQuery args), argCountry = Text.strip <$> argCountry args}
  where
    invalid = Left (failure "windguru_invalid_arguments")
    validText cap value = let count = Text.length (Text.strip value) in count >= 2 && count <= cap && Text.all (\char -> char >= ' ' && char /= '\DEL') value

validForecast :: WindguruRules -> ForecastArgs -> Result ForecastQuery
validForecast rules args
  | argSpotIdForecast args < 1 || argSpotIdForecast args > windguruSpotLimit rules = invalid
  | argModelIdForecast args < 1 || argModelIdForecast args > windguruModelLimit rules = invalid
  | otherwise = ForecastQuery (argSpotIdForecast args) (argModelIdForecast args) <$> parseWindow rules (argHours args) (argStart args)
  where invalid = Left (failure "windguru_invalid_arguments")

data WindguruRequest = SearchSpots SearchArgs | SpotMetadata Int | SpotForecast ForecastQuery | SavedForecasts Window

parseRequest :: WindguruRules -> Text -> Value -> Result WindguruRequest
parseRequest rules name arguments = case name of
  "windguru_search_spots" -> SearchSpots <$> (decodeValue "windguru_invalid_arguments" arguments >>= validSearch rules)
  "windguru_spot" -> SpotMetadata <$> (decodeValue "windguru_invalid_arguments" arguments >>= validSpot)
  "windguru_forecast" -> SpotForecast <$> (decodeValue "windguru_invalid_arguments" arguments >>= validForecast rules)
  "windguru_forecast_set" -> SavedForecasts <$> (decodeValue "windguru_invalid_arguments" arguments >>= validSet)
  _ -> Left (failure "windguru_unknown_tool")
  where
    validSpot args
      | argSpotId args >= 1 && argSpotId args <= windguruSpotLimit rules = Right (argSpotId args)
      | otherwise = Left (failure "windguru_invalid_arguments")
    validSet args = parseWindow rules (setHours args) (setStart args)

execute :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> IO (Result Text) -> Execute
execute rules send clock local readSet name arguments _ = continue run (parseRequest rules name arguments)
  where
    run (SearchSpots args) = bounded $ do
      let wire = request rules "search_spots" [("search", argQuery args)]
      response <- mapResult (decodeReceived "windguru_invalid_response") (send wire)
      pure (fmap toJSON (response >>= search rules args))
    run (SpotMetadata ident) = bounded $ do
      let wire = request rules "forecast_spot" [("id_spot", Text.pack (show ident))]
      response <- mapResult (decodeReceived "windguru_invalid_response") (send wire)
      let selected = response >>= getSpot rules ident
      continue catalog selected
    run (SpotForecast query) = bounded (getForecast rules send clock local query)
    run (SavedForecasts window) = forecastSet rules send clock local readSet window
    catalog value = do
      let wire = request rules "model_info_full" []
      models <- mapResult (decodeReceived "windguru_invalid_response") (send wire)
      pure (Right (toJSON (metadata rules local models value)))
    bounded = within deadline micros
    deadline = failure "windguru_timeout"
    micros = windguruHttpTimeout rules * 1000000

validNumericId :: Int -> Numeric -> Bool
validNumericId cap value = number >= 1 && number <= fromIntegral cap && number == fromInteger (round number)
  where number = numericValue value

validateSpot :: WindguruRules -> Int -> RawSpot -> Result RawSpot
validateSpot rules ident value
  | not (validNumericId (windguruSpotLimit rules) (rawIdSpot value)) || round (numericValue (rawIdSpot value)) /= ident = invalid
  | abs (rawLat value) > 90 || abs (rawLon value) > 180 = invalid
  | length (rawModels value) > windguruProviderRows rules || not (all (validNumericId (windguruModelLimit rules)) (rawModels value)) = invalid
  | otherwise = Right value {rawModels = nub (rawModels value)}
  where invalid = Left (failure "windguru_invalid_response")

validateSet :: WindguruRules -> ForecastSet -> Result ForecastSet
validateSet rules value
  | length tabs > windguruProviderRows rules = Left (failure "windguru_forecast_set_too_large")
  | otherwise = value <$ traverse (validateTab rules) (zip [0..] tabs)
  where tabs = setDataTabs value

validateTab :: WindguruRules -> (Int, SetTab) -> Result ()
validateTab rules (index, tab)
  | index < windguruMaximumSetPairs rules && not (all (maybe True validModel) optionalModels) = invalid
  | null models || length models > windguruProviderRows rules || not (all validModel models) = invalid
  | otherwise = Right ()
  where
    models = map selectedIdModel (tabIdModelArr tab)
    optionalModels = [tabIdModel tab, tabIdModelWave tab]
    validModel = validNumericId (windguruModelLimit rules)
    invalid = Left (failure "windguru_invalid_response")

withZone :: LocalZone -> UTCTime -> RawSpot -> RawSpot
withZone local now value = value {rawTzid = selected, rawTz = Nothing}
  where
    candidate = case rawTzid value of
      Just zone | not (Text.null zone) -> Just zone
      _ -> rawTz value
    selected = candidate >>= \zone -> either (const Nothing) (const (Just zone)) (local zone now)


decodeSet :: WindguruRules -> Received Value -> Result (Received ForecastSet)
decodeSet rules received = do
  frame <- decodeValue "windguru_forecast_set_unavailable" (receivedValue received)
  tabs <- traverse (decodeValue "windguru_invalid_response") (frameTabs frame)
  ids <- traverse (setSpotId rules) tabs
  spots <- traverse (decodeSetSpot rules (frameSpots frame)) (nub ids)
  let title = frameSet frame >>= parseMaybe parseJSON
      value = ForecastSet title (Map.fromList spots) tabs
  Right (Received value (receivedAt received))

setSpotId :: WindguruRules -> SetTab -> Result Int
setSpotId rules tab
  | validNumericId (windguruSpotLimit rules) (tabIdSpot tab) = Right (round (numericValue (tabIdSpot tab)))
  | otherwise = Left (failure "windguru_invalid_response")

decodeSetSpot :: WindguruRules -> Map Text Value -> Int -> Result (Text, RawSpot)
decodeSetSpot rules spots ident = do
  let key = Text.pack (show ident)
  payload <- maybe (Left (failure "windguru_spot_not_found")) Right (Map.lookup key spots)
  raw <- decodeValue "windguru_invalid_response" payload
  value <- validateSpot rules ident raw
  Right (key, value)
