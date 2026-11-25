module LiveCheck.Providers.Public (mapsRead, windguruRead, windguruSavedRead) where

import Baerbel.Core.Calendar (TimeZones(..))
import Baerbel.Core.Config (Settings, rule, setting, booleanSetting)
import Baerbel.Core.Error (Failure(..), Result)
import Baerbel.Core.JSON (field, textField, jsonOptions)
import Baerbel.Core.Services (mapsAuthorization, readForecastSet)
import Baerbel.Core.TimeFormat (isoUtc)
import Baerbel.Core.TimeZones (timeZones)
import Baerbel.Runtime.HTTP (Send)
import Baerbel.Runtime.Maps (maps)
import Baerbel.Runtime.Provider (Execute)
import Baerbel.Runtime.Profile (ProfileStore(profileRead))
import Baerbel.Runtime.Store (profileStore)
import Baerbel.Runtime.Windguru (windguru)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson (Value(..), ToJSON(..), genericToJSON, object, (.=))
import Data.Text (Text)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import LiveCheck.Check (metered, emit)
import LiveCheck.Providers.Common
import qualified Data.Text as Text

mapsRead :: Settings -> Send -> IO (Result Value)
mapsRead settings sender = runExceptT $ do
  rules <- except (rule settings "providers" "maps")
  routes <- except (rule settings "providers" "route")
  elevation <- except (booleanSetting settings "PHONE_ASSISTANT_MAPS_ELEVATION")
  let key = setting settings "GOOGLE_MAPS_API_KEY"
      authorized request = sender (mapsAuthorization key request)
      redact = Text.replace key "[redacted]"
  (counted, requests) <- lift (metered authorized)
  execute <- lift (maps rules routes redact counted elevation getCurrentTime)
  ExceptT (mapWorkflow execute requests elevation)

mapWorkflow :: Execute -> IO Int -> Bool -> IO (Result Value)
mapWorkflow execute requests elevation = runExceptT $ do
  (origin, destination, cold, warm) <- ExceptT (landmarks execute requests)
  let args = object ["place_ref" .= origin]
  details <- ExceptT (tool execute "maps_place_details" args)
  route <- ExceptT (walkLandmarks execute origin destination)
  total <- lift requests
  except (mapReport elevation cold warm total details route)

landmarks :: Execute -> IO Int -> IO (Result (Text, Text, Double, Double))
landmarks execute requests = runExceptT $ do
  (origin, cold) <- ExceptT (timed (findLandmark execute "British Museum"))
  before <- lift requests
  (destination, warm) <- ExceptT (timed (findLandmark execute "National Gallery"))
  after <- lift requests
  except (ensure "live_maps_area_cache_failed" (before == 2 && after == 3))
  pure (origin, destination, cold, warm)

findLandmark :: Execute -> Text -> IO (Result Text)
findLandmark execute name = runExceptT $ do
  let args = object ["query" .= name, "area" .= ("London, United Kingdom" :: Text)]
  result <- ExceptT (tool execute "maps_find_place" args)
  candidate <- except (member "candidates" result >>= firstItem)
  source <- except (member "source" candidate :: Result Text)
  except (ensure "live_maps_place_search_fallback" (source == "google_places"))
  except (member "ref" candidate)

walkLandmarks :: Execute -> Text -> Text -> IO (Result Value)
walkLandmarks execute origin destination = tool execute "maps_route" args
  where args = object ["origin_ref" .= origin, "destination_ref" .= destination]

mapReport :: Bool -> Double -> Double -> Int -> Value -> Value -> Result Value
mapReport elevation cold warm requests details route = do
  distance <- member "distance_m" route :: Result Int
  duration <- member "duration_s" route :: Result Double
  steps <- member "step_count" route :: Result Int
  complete <- member "directions_complete" route
  ensure "live_maps_route_incomplete" (distance > 0 && duration > 0 && steps > 0 && complete)
  ensure "live_maps_details_incomplete" (isJust (textField "place_ref" details))
  let terrain = field "terrain" route >>= textField "status"
  ensure "live_maps_elevation_unavailable" (not elevation || terrain == Just "estimated")
  Right (object ["status" .= ("ok" :: Text), "requests" .= requests, "distance_m" .= distance,
    "duration_s" .= duration, "step_count" .= steps, "directions_complete" .= complete,
    "area_cache_verified" .= True, "cold_search_seconds" .= cold, "warm_search_seconds" .= warm,
    "elevation_enabled" .= elevation, "elevation_status" .= terrain])

windguruRead :: Settings -> Send -> IO (Result Value)
windguruRead settings sender = runExceptT $ do
  (counted, requests) <- lift (metered sender)
  execute <- ExceptT (savedExecutor settings counted)
  spot <- ExceptT (findSpot execute)
  model <- ExceptT (findModel execute spot)
  individual <- ExceptT (forecastWorkflow execute requests spot model)
  saved <- ExceptT (savedForecastWorkflow execute)
  pure (object ["individual" .= individual, "saved_set" .= saved])

findSpot :: Execute -> IO (Result Int)
findSpot execute = runExceptT $ do
  let args = object ["query" .= ("Poole Harbour" :: Text), "country" .= Null, "limit" .= (3 :: Int)]
  result <- ExceptT (tool execute "windguru_search_spots" args)
  candidate <- except (member "candidates" result >>= firstItem)
  except (member "spot_id" candidate)

findModel :: Execute -> Int -> IO (Result Int)
findModel execute spot = runExceptT $ do
  let args = object ["spot_id" .= spot]
  result <- ExceptT (tool execute "windguru_spot" args)
  except (ensure "live_windguru_model_catalog_failed" (isNothing (textField "model_catalog_error" result)))
  models <- except (member "models" result :: Result [Value])
  model <- except (firstItem (filter ((== Just "weather") . textField "kind") models))
  except (member "model_id" model)

forecastWorkflow :: Execute -> IO Int -> Int -> Int -> IO (Result Value)
forecastWorkflow execute requests spot model = runExceptT $ do
  now <- lift getCurrentTime
  let args = object ["spot_id" .= spot, "model_id" .= model, "start" .= isoUtc now, "hours" .= (6 :: Int)]
  (initial, cold) <- ExceptT (timed (tool execute "windguru_forecast" args))
  before <- lift requests
  (cached, warm) <- ExceptT (timed (tool execute "windguru_forecast" args))
  after <- lift requests
  except (ensure "live_windguru_cache_failed" (before == after && field "retrieved_at" initial == field "retrieved_at" cached))
  except (forecastReport after cold warm cached)

forecastReport :: Int -> Double -> Double -> Value -> Result Value
forecastReport requests cold warm value = do
  count <- member "point_count" value :: Result Int
  stale <- member "older_than_24_hours" value :: Result Bool
  points <- member "points" value :: Result [Value]
  ensure "live_windguru_forecast_empty" (count > 0 && count == length points)
  ensure "live_windguru_forecast_missing_wind" (any hasWind points)
  Right (object ["status" .= ("ok" :: Text), "point_count" .= count, "older_than_24_hours" .= stale,
    "requests" .= requests, "cache_verified" .= True, "cold_forecast_seconds" .= cold, "warm_forecast_seconds" .= warm])
  where hasWind point = case field "wind_knots" point of
          Just (Number _) -> True
          _ -> False

data SavedWindow = SavedWindow
  { windowName :: Text, windowStatus :: Text, windowForecastCount :: Int
  , windowAvailableForecasts :: Int, windowEmptyForecasts :: Int, windowReturnedPoints :: Int
  , windowFailureCodes :: Map.Map Text Int } deriving (Generic)

instance ToJSON SavedWindow where toJSON = genericToJSON (jsonOptions 6)

windguruSavedRead :: Settings -> Send -> IO (Result Value)
windguruSavedRead settings sender = runExceptT $ do
  execute <- ExceptT (savedExecutor settings sender)
  ExceptT (savedForecastWorkflow execute)

savedExecutor :: Settings -> Send -> IO (Result Execute)
savedExecutor settings sender = runExceptT $ do
  rules <- except (rule settings "providers" "windguru")
  profileRules <- except (rule settings "providers" "profile")
  storeRules <- except (rule settings "profile" "store")
  let path = Text.unpack (setting settings "PHONE_ASSISTANT_PROFILE_FILE")
  store <- ExceptT (profileStore profileRules storeRules path)
  let saved = readForecastSet . fmap Just <$> profileRead store
  lift (windguru rules sender getCurrentTime (zonedInstant timeZones) saved)

savedForecastWorkflow :: Execute -> IO (Result Value)
savedForecastWorkflow execute = runExceptT $ do
  now <- lift getCurrentTime
  current <- lift (savedWindow execute now ("current", 0, 6))
  later <- lift (traverse (savedWindow execute now) [("tomorrow", 24, 24), ("multiday", 0, 72)])
  let rows = current : later
      report = object ["windows" .= rows]
      valid = all ((== "ok") . windowStatus) rows
  lift (emit report)
  except (ensure "live_windguru_saved_set_failed" valid)
  except (ensure "live_windguru_saved_set_no_current_points" (windowAvailableForecasts current > 0))
  pure report

savedWindow :: Execute -> UTCTime -> (Text, Int, Int) -> IO SavedWindow
savedWindow execute now (name, offset, hours) = do
  let start = addUTCTime (fromIntegral offset * 3600) now
      arguments = object ["start" .= isoUtc start, "hours" .= hours]
  result <- tool execute "windguru_forecast_set" arguments
  pure (either (failedWindow name) id (result >>= savedWindowReport name))

failedWindow :: Text -> Failure -> SavedWindow
failedWindow name issue = SavedWindow name "error" 0 0 0 0 codes
  where codes = Map.singleton (failureCode issue) 1

savedWindowReport :: Text -> Value -> Result SavedWindow
savedWindowReport name result = do
  forecasts <- member "forecasts" result :: Result [Value]
  let successfulRows = filter ((== Just "ok") . textField "status") forecasts
      failures = mapMaybe (textField "error") forecasts
      codes = Map.fromListWith (+) [(code, 1) | code <- failures]
  counts <- traverse (member "returned_point_count") successfulRows :: Result [Int]
  let available = length (filter (> 0) counts)
      empty = length (filter (== 0) counts)
  Right (SavedWindow name "ok" (length forecasts) available empty (sum counts) codes)
