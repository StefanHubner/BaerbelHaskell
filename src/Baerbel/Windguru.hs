{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Windguru (windguru, forecast, Numeric(..)) where

import Baerbel.TimeFormat (isoUtc, isoZoned)
import Data.Either (rights)
import Data.Char (isDigit)
import GHC.Clock (getMonotonicTimeNSec)
import Baerbel.Cache (memoize)
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Body(..), Request(..), Send)
import Baerbel.JSON (argumentOptions, jsonOptions, decodeValue)
import Baerbel.Provider hiding (fetch)
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
  let title = if Text.null name then longName else name
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

startTime :: WindguruRules -> UTCTime -> Maybe Text -> Result UTCTime
startTime _ now Nothing = Right now
startTime rules _ (Just value)
  | Text.length value > windguruStartChars rules = Left (failure "windguru_invalid_arguments")
  | otherwise = maybe (Left (failure "windguru_invalid_arguments")) (Right . zonedTimeToUTC) parsed
  where
    normalized = if "Z" `Text.isSuffixOf` value then Text.dropEnd 1 value <> "+00:00" else value
    parsed = parseTimeM True defaultTimeLocale "%FT%T%Q%Ez" (Text.unpack normalized)

forecast :: WindguruRules -> (Text -> UTCTime -> Result ZonedTime) -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> RawForecast -> Result Forecast
forecast rules local now raw model start hours response = do
  dataValue <- case rawForecastFcst response of
    Just value@(Object _) -> decodeValue "windguru_invalid_response" value
    _ -> Left (failure "windguru_forecast_unavailable")
  let selectedFields = Map.filterWithKey (\key value -> key `elem` fieldNames && value /= Null) (forecastFields dataValue)
      fieldNames = map (\(name, _, _, _) -> name) (windguruFields rules)
  arrays <- traverse (decodeValue "windguru_invalid_response") selectedFields
  normalizeForecast rules local now raw model start hours response dataValue arrays

normalizeForecast :: WindguruRules -> (Text -> UTCTime -> Result ZonedTime) -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> RawForecast -> ForecastData -> Map Text [Value] -> Result Forecast
normalizeForecast rules local now raw model start hours response dataValue arrays
  | not (validNumericId (windguruSpotLimit rules) (rawForecastIdSpot response)) || not (validNumericId (windguruModelLimit rules) (rawForecastIdModel response)) = invalid
  | stamp < windguruMinimumTimestamp rules || stamp > windguruMaximumTimestamp rules || any (\hour -> hour < 0 || hour > windguruMaximumSourceHours rules) sourceHours = invalid
  | not (any (`Map.member` arrays) (windguruRequiredFields rules)) = Left (failure "windguru_forecast_unavailable")
  | round (numericValue (rawForecastIdSpot response)) /= spotSpotId identity || round (numericValue (rawForecastIdModel response)) /= model = Left (failure "windguru_invalid_response")
  | null sourceHours || length sourceHours > windguruMaximumSourcePoints rules || not (and (zipWith (<) sourceHours (drop 1 sourceHours))) = Left (failure "windguru_invalid_response")
  | any ((/= length sourceHours) . length) (Map.elems arrays) = Left (failure "windguru_invalid_response")
  | initialized > addUTCTime (realToFrac (windguruFutureToleranceSeconds rules)) now = Left (failure "windguru_invalid_response")
  | otherwise = do
      points <- traverse render selected
      let age :: Double
          age = max 0 (realToFrac (diffUTCTime now initialized) / 3600)
          availableFirst = at (firstOr 0 sourceHours)
          availableLast = at (foldl (\_ value -> value) 0 sourceHours)
          tide = Tide False Nothing (windguruTideReason rules)
      Right (Forecast "ok" "windguru_public_forecast" identity model (clean (windguruModelChars rules) (forecastModelName dataValue)) (iso now) (iso initialized)
        (fromIntegral (round (age * 100) :: Int) / 100) (age > 24) (iso start) (iso finish) (iso availableFirst) (iso availableLast) points (length points) truncated
        (if null points then "no_points_in_requested_window" else "available") units conventions (sun rules raw) tide True)
  where
    identity = spot rules raw
    invalid = Left (failure "windguru_invalid_response")
    stamp = forecastInitstamp dataValue
    initialized = posixSecondsToUTCTime (realToFrac stamp)
    sourceHours = forecastHours dataValue
    at hour = addUTCTime (realToFrac (hour * 3600)) initialized
    finish = addUTCTime (fromIntegral hours * 3600) start
    available = filter (\(_, hour) -> at hour >= start && at hour < finish) (zip [0..] sourceHours)
    selected = take (windguruMaximumPoints rules) available
    truncated = length available > windguruMaximumPoints rules
    units = windguruUnits rules
    conventions = Map.fromList [("wind", windguruWindDirectionConvention rules), ("waves", windguruWaveDirectionConvention rules)]
    render (index, hour) = do
      values <- traverse (fieldValue index) (windguruFields rules)
      let numeric = Map.fromListWith prefer (catMaybes values)
          instant = at hour
          localTime = spotTimezone identity >>= either (const Nothing) (Just . isoZoned) . (`local` instant)
          timestamp = TimePoint (iso instant) localTime
      Right (ForecastPoint timestamp numeric)
    prefer later Nothing = later
    prefer _ earlier = earlier
    fieldValue index (source, target, lower, upper) = case Map.lookup source arrays of
      Nothing -> Right Nothing
      Just values -> Just . (target,) <$> readingNumber lower upper (firstOr Null (drop index values))

readingNumber :: Double -> Double -> Value -> Result (Maybe Double)
readingNumber lower upper raw = do
  reading <- decodeValue "windguru_invalid_response" raw
  case readingValue reading of
    Nothing -> Right Nothing
    Just value | value >= lower && value <= upper -> Right (Just value)
    _ -> Left (failure "windguru_invalid_response")

data Received a = Received {receivedValue :: a, receivedAt :: UTCTime}
type TimedSend = Request -> IO (Result (Received Value))

fetch :: FromJSON a => Text -> IO (Result (Received Value)) -> IO (Result (Received a))
fetch code = fmap (>>= decodeReceived)
  where decodeReceived value = (`Received` receivedAt value) <$> decodeValue code (receivedValue value)

request :: WindguruRules -> TimedSend -> Text -> [(Text, Text)] -> IO (Result (Received Value))
request rules send query params = send wire
  where
    queryParams = textParam "q" query : map (\(key, value) -> textParam (Text.encodeUtf8 key) value) params
    suffix = if query == "forecast_set" then ("?set=" <>) <$> lookup "set" params else lookup "id_spot" params
    referer = windguruBaseUrl rules <> fromMaybe "" suffix
    headers = [("Accept", "application/json"), ("Referer", Text.encodeUtf8 referer), ("User-Agent", Text.encodeUtf8 (windguruUserAgent rules))]
    wire = Request "GET" (windguruApiUrl rules) headers queryParams EmptyBody (windguruHttpBytes rules) (windguruHttpTimeout rules)

getSpot :: WindguruRules -> TimedSend -> (Text -> UTCTime -> Result ZonedTime) -> Int -> IO (Result (Received RawSpot))
getSpot rules send local ident = do
  response <- fetch "windguru_invalid_response" operation
  pure (response >>= select)
  where
    text = Text.pack (show ident)
    operation = request rules send "forecast_spot" [("id_spot", text)]
    select value = do
      payload <- maybe (Left (failure "windguru_spot_not_found")) Right (Map.lookup text (metaSpots (receivedValue value)))
      raw <- decodeValue "windguru_invalid_response" payload
      validatedSpot <- validateSpot rules ident raw
      Right (Received (withZone local (receivedAt value) validatedSpot) (receivedAt value))

search :: WindguruRules -> TimedSend -> SearchArgs -> IO (Result Value)
search rules send args = do
  response <- fetch "windguru_invalid_response" operation
  pure (response >>= fmap toJSON . normalize)
  where
    query = Text.strip (argQuery args)
    operation = request rules send "search_spots" [("search", query)]
    normalize received
      | searchCount value < length rows || not (all (validNumericId (windguruSpotLimit rules) . rowIdSpot) rows) = Left (failure "windguru_invalid_response")
      | otherwise = Right (SearchResult "ok" "windguru_public_search" query (argCountry args) (take (windguruCountryLimit rules) countries)
      (length countries > windguruCountryLimit rules) unknown candidates (length candidates) (length matched) providerCut
      (length matched > argLimit args || providerCut) (iso now) scope True)
      where
        value = receivedValue received
        now = receivedAt received
        rows = map normalizeRow (searchSpots value)
        countries = nubBy (\first second -> Text.toCaseFold first == Text.toCaseFold second) (filter (not . Text.null) (mapMaybe rowCountry rows))
        unknown = length (filter (maybe True Text.null . rowCountry) rows)
        normalizeRow item = item {rowCountry = clean (windguruCountryChars rules) <$> rowCountry item}
        matches row = maybe True (\country -> fmap Text.toCaseFold (rowCountry row) == Just (Text.toCaseFold country)) (argCountry args)
        matched = filter matches rows
        candidates = map candidate (take (argLimit args) matched)
        providerCut = searchCount value > length rows
        scope = windguruSearchScope rules
    candidate row = Candidate ident (clean (windguruNameChars rules) (fromMaybe "" (rowSpotname row))) (rowCountry row) url
      where ident = round (numericValue (rowIdSpot row))
            url = windguruBaseUrl rules <> Text.pack (show ident)

metadata :: WindguruRules -> TimedSend -> (Text -> UTCTime -> Result ZonedTime) -> SpotArgs -> IO (Result Value)
metadata rules send local args = do
  value <- getSpot rules send local (argSpotId args)
  either (pure . Left) withCatalog value
  where
    withCatalog value = do
      catalog <- fetch "windguru_invalid_response" (request rules send "model_info_full" [])
      pure (Right (toJSON (finish catalog value)))
    finish catalog received = SpotResult "ok" "windguru_public_spot" (spot rules value) (iso now) models problem (sun rules value) tide True
      where
        value = receivedValue received
        now = receivedAt received
        models = map (modelInfo catalog . round . numericValue) (rawModels value)
        problem = either (Just . failureCode) (const Nothing) catalog
        tide = Tide False (Just (fromMaybe False (rawTide value >>= parseMaybe (withObject "tide" (pure . not . KeyMap.null))))) (windguruMetadataTideReason rules)
    modelInfo catalog ident = Model ident name kind virtual resolution
      where
        item = either (const Nothing) (Map.lookup (Text.pack (show ident)) . receivedValue) catalog
        name = clean (windguruModelChars rules) (fromMaybe "" (item >>= modelModelName))
        wave = flagValue <$> (item >>= modelWave)
        kind = case wave of
          Nothing -> "unknown"
          Just True -> "wave"
          Just False -> "weather"
        virtual = flagValue <$> (item >>= modelVirtual)
        resolution = item >>= modelResolution >>= \value -> if value >= 0 && value <= windguruResolutionLimit rules then Just value else Nothing

getForecast :: WindguruRules -> TimedSend -> IO UTCTime -> (Text -> UTCTime -> Result ZonedTime) -> ForecastArgs -> IO (Result Value)
getForecast rules send clock local args = do
  value <- getSpot rules send local (argSpotIdForecast args)
  either (pure . Left) (finish . receivedValue) value
  where
    finish raw
      | argModelIdForecast args `notElem` map (round . numericValue) (rawModels raw) = pure (Left (failure "windguru_model_unavailable_for_spot"))
      | otherwise = do
          response <- fetch "windguru_invalid_response" operation
          now <- clock
          let result = startTime rules now (argStart args) >>= \start -> response >>= normalizeReceived rules local now raw (argModelIdForecast args) start (argHours args)
          pure (fmap toJSON result)
    operation = request rules send "forecast" [("id_spot", Text.pack (show (argSpotIdForecast args))), ("id_model", Text.pack (show (argModelIdForecast args)))]

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

type SelectedPair = (Int, Int, Bool)

setEntries :: WindguruRules -> ForecastSet -> ([SelectedPair], Selection)
setEntries rules value = (selected, selection)
  where
    tabs = setDataTabs value
    pairs = [(round (numericValue (tabIdSpot tab)), round (numericValue (selectedIdModel model)), maybe False (not . Text.null) (tabProOnlyMessage tab)) | tab <- tabs, model <- tabIdModelArr tab]
    same (spotA, modelA, _) (spotB, modelB, _) = spotA == spotB && modelA == modelB
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
  (outTruncated value) (outTruncated value || returned < available) (if null omitted then Nothing else Just omitted)
  where
    points = outPoints value
    available = length points
    returned = min available count
    selected = take count points
    localTimes = not (null points) && all (isJust . timeTimeLocal . pointTime) points
    timeKey = if localTimes then "time_local" else "time_utc"
    timestamp pointValue = if localTimes then toJSON (timeTimeLocal (pointTime pointValue)) else toJSON (timeTimeUtc (pointTime pointValue))
    present key = any (Map.member key . pointNumbers) points
    keys = filter present (windguruComparisonColumns rules)
    numeric key pointValue = toJSON (fromMaybe Nothing (Map.lookup key (pointNumbers pointValue)))
    columns = Map.fromList ((timeKey, map timestamp selected) : [(key, map (numeric key) selected) | key <- keys])
    omitted = filter present (windguruOmittedColumns rules)

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

normalizeReceived :: WindguruRules -> (Text -> UTCTime -> Result ZonedTime) -> UTCTime -> RawSpot -> Int -> UTCTime -> Int -> Received RawForecast -> Result Forecast
normalizeReceived rules local now raw model start hours received = do
  value <- forecast rules local now (withZone local now raw) model start hours (receivedValue received)
  Right value {outRetrievedAt = iso (receivedAt received)}

newtype Deadline = Deadline Double

newDeadline :: Int -> IO Deadline
newDeadline seconds = do
  now <- getMonotonicTimeNSec
  let end = fromIntegral now / 1000000000 + fromIntegral seconds
  pure (Deadline end)

beforeDeadline :: Deadline -> IO a -> IO (Maybe a)
beforeDeadline (Deadline end) action = do
  now <- getMonotonicTimeNSec
  let remaining = max 0 (end - fromIntegral now / 1000000000)
      micros = floor (remaining * 1000000)
  timeout micros action

forecastSet :: WindguruRules -> TimedSend -> IO UTCTime -> (Text -> UTCTime -> Result ZonedTime) -> IO (Result Text) -> SetArgs -> IO (Result Value)
forecastSet rules send clock local readSet args = do
  deadline <- newDeadline (windguruHttpTimeout rules)
  saved <- beforeDeadline deadline readSet
  either (pure . Left) (load deadline) (fromMaybe timedOut saved)
  where
    timedOut = Left (failure "windguru_timeout")
    load deadline text = case readMaybe (Text.unpack (Text.strip text)) of
      Nothing -> pure (Left (failure "windguru_invalid_forecast_set_id"))
      Just ident | ident < 1 || ident > windguruSpotLimit rules -> pure (Left (failure "windguru_invalid_forecast_set_id"))
      Just ident -> do
        let parameter = Text.pack (show (ident :: Int))
        response <- beforeDeadline deadline (fetchSet rules (request rules send "forecast_set" [("set", parameter)]))
        either (pure . Left) (retrieve deadline ident) (fromMaybe (Left (failure "windguru_timeout")) response)
    retrieve deadline ident received = do
      now <- clock
      let value = receivedValue received
          prepared = (,) <$> validateSet rules value <*> startTime rules now (setStart args)
      either (pure . Left) (members deadline ident received) prepared
    members deadline ident received (value, start) = do
      let (selected, selection) = setEntries rules value
      results <- boundedMap (windguruSetConcurrency rules) (member deadline value start) selected
      let ended = any fst results
          values = map snd results
      pure (finishSet rules ident value (receivedAt received) start (setHours args) selection ended values)
    member deadline value start (spotId, modelId, restricted) = case Map.lookup (Text.pack (show spotId)) (setDataSpots value) of
      Nothing -> pure (False, Left (FailedForecast "error" "windguru_spot_not_found" modelId spotId (Spot spotId "" Nothing 0 0 Nothing "")))
      Just raw -> fetchMember deadline raw start modelId restricted
    fetchMember deadline raw start modelId restricted
      | restricted = pure (False, Left (failed raw modelId "windguru_forecast_requires_login_or_pro"))
      | modelId `notElem` map (round . numericValue) (rawModels raw) = pure (False, Left (failed raw modelId "windguru_model_unavailable_for_spot"))
      | otherwise = do
          let spotId = round (numericValue (rawIdSpot raw)) :: Int
              params = [("id_spot", Text.pack (show spotId)), ("id_model", Text.pack (show modelId))]
          response <- beforeDeadline deadline (fetch "windguru_invalid_response" (request rules send "forecast" params))
          now <- clock
          let decoded = fromMaybe (Left (failure "windguru_timeout")) response
              result = decoded >>= normalizeReceived rules local now raw modelId start (setHours args)
          pure (isNothing response, either (Left . failed raw modelId . failureCode) Right result)
    failed raw modelId code = FailedForecast "error" code modelId (spotSpotId (spot rules raw)) (spot rules raw)

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
  if referenceByteSize output > windguruResultBytes rules then Left (failure "windguru_forecast_set_result_too_large") else Right output

windguru :: WindguruRules -> Send -> IO UTCTime -> (Text -> UTCTime -> Result ZonedTime) -> IO (Result Text) -> IO Execute
windguru rules send clock local readSet = do
  cached <- memoize (windguruCacheEntries rules) (windguruCacheSeconds rules) key retrieve
  pure (execute rules cached clock local readSet)
  where
    key requestValue = (requestUrl requestValue, requestQuery requestValue)
    retrieve requestValue = do
      response <- send requestValue
      either (pure . Left) timestamp (response >>= apiResult)
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

validForecast :: WindguruRules -> ForecastArgs -> Result ForecastArgs
validForecast rules args
  | argSpotIdForecast args < 1 || argSpotIdForecast args > windguruSpotLimit rules = invalid
  | argModelIdForecast args < 1 || argModelIdForecast args > windguruModelLimit rules = invalid
  | argHours args < 1 || argHours args > windguruMaximumPoints rules = invalid
  | otherwise = args <$ startTime rules epoch (argStart args)
  where
    invalid = Left (failure "windguru_invalid_arguments")
    epoch = posixSecondsToUTCTime 0

execute :: WindguruRules -> TimedSend -> IO UTCTime -> (Text -> UTCTime -> Result ZonedTime) -> IO (Result Text) -> Execute
execute rules send clock local readSet name arguments _ = case name of
  "windguru_search_spots" -> parsed (validSearch rules) (search rules send)
  "windguru_spot" -> parsed validSpot (metadata rules send local)
  "windguru_forecast" -> parsed (validForecast rules) (getForecast rules send clock local)
  "windguru_forecast_set" -> parsed validSet (forecastSet rules send clock local readSet)
  _ -> pure (Left (failure "windguru_unknown_tool"))
  where
    validSpot args
      | argSpotId args >= 1 && argSpotId args <= windguruSpotLimit rules = Right args
      | otherwise = Left (failure "windguru_invalid_arguments")
    validSet args
      | setHours args >= 1 && setHours args <= windguruMaximumPoints rules = args <$ startTime rules (posixSecondsToUTCTime 0) (setStart args)
      | otherwise = Left (failure "windguru_invalid_arguments")
    parsed :: FromJSON a => (a -> Result a) -> (a -> IO (Result Value)) -> IO (Result Value)
    parsed validate operation = either (pure . Left) bounded (decodeValue "windguru_invalid_arguments" arguments >>= validate)
      where
        bounded args
          | name == "windguru_forecast_set" = operation args
          | otherwise = fmap (fromMaybe (Left (failure "windguru_timeout"))) (timeout (windguruHttpTimeout rules * 1000000) (operation args))


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
  | otherwise = value <$ traverse validateTab (zip [0 :: Int ..] tabs)
  where
    tabs = setDataTabs value
    validateTab (index, tab) = do
      let spotId = tabIdSpot tab
          models = map selectedIdModel (tabIdModelArr tab)
          invalid = Left (failure "windguru_invalid_response")
          optionalModels = [tabIdModel tab, tabIdModelWave tab]
      if index < windguruMaximumSetPairs rules && not (all (maybe True (validNumericId (windguruModelLimit rules))) optionalModels) then invalid else Right ()
      if validNumericId (windguruSpotLimit rules) spotId && not (null models) && length models <= windguruProviderRows rules && all (validNumericId (windguruModelLimit rules)) models then Right () else invalid
      raw <- maybe (Left (failure "windguru_spot_not_found")) Right (Map.lookup (Text.pack (show (round (numericValue spotId) :: Int))) (setDataSpots value))
      validateSpot rules (round (numericValue spotId)) raw


withZone :: (Text -> UTCTime -> Result ZonedTime) -> UTCTime -> RawSpot -> RawSpot
withZone local now value = value {rawTzid = selected, rawTz = Nothing}
  where
    candidate = case rawTzid value of
      Just zone | not (Text.null zone) -> Just zone
      _ -> rawTz value
    selected = candidate >>= \zone -> either (const Nothing) (const (Just zone)) (local zone now)


fetchSet :: WindguruRules -> IO (Result (Received Value)) -> IO (Result (Received ForecastSet))
fetchSet rules = fmap (>>= decodeSet rules)

decodeSet :: WindguruRules -> Received Value -> Result (Received ForecastSet)
decodeSet rules received = do
  frame <- decodeValue "windguru_forecast_set_unavailable" (receivedValue received)
  tabs <- traverse (decodeValue "windguru_invalid_response") (frameTabs frame)
  if all (validNumericId (windguruSpotLimit rules) . tabIdSpot) tabs then Right () else Left (failure "windguru_invalid_response")
  let ids = nub (map (round . numericValue . tabIdSpot) tabs)
  spots <- traverse (decodeSetSpot rules (frameSpots frame)) ids
  let title = frameSet frame >>= parseMaybe parseJSON
      value = ForecastSet title (Map.fromList spots) tabs
  Right (Received value (receivedAt received))

decodeSetSpot :: WindguruRules -> Map Text Value -> Int -> Result (Text, RawSpot)
decodeSetSpot rules spots ident = do
  let key = Text.pack (show ident)
  payload <- maybe (Left (failure "windguru_spot_not_found")) Right (Map.lookup key spots)
  raw <- decodeValue "windguru_invalid_response" payload
  value <- validateSpot rules ident raw
  Right (key, value)
