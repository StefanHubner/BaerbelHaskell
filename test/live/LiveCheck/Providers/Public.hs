module LiveCheck.Providers.Public (mapsRead, windguruRead) where

import Baerbel.Core.Calendar (TimeZones(..))
import Baerbel.Core.Config (Settings, rule, setting, booleanSetting)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (field, textField)
import Baerbel.Core.Services (mapsAuthorization)
import Baerbel.Core.TimeFormat (isoUtc)
import Baerbel.Core.TimeZones (timeZones)
import Baerbel.Runtime.HTTP (Send)
import Baerbel.Runtime.Maps (maps)
import Baerbel.Runtime.Provider (Execute)
import Baerbel.Runtime.Windguru (windguru)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import Data.Maybe (isJust, isNothing)
import Data.Time (getCurrentTime)
import LiveCheck.Check (metered)
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
  rules <- except (rule settings "providers" "windguru")
  (counted, requests) <- lift (metered sender)
  let noSavedSet = pure (Left (failure "live_saved_forecast_set_not_requested"))
  execute <- lift (windguru rules counted getCurrentTime (zonedInstant timeZones) noSavedSet)
  spot <- ExceptT (findSpot execute)
  model <- ExceptT (findModel execute spot)
  ExceptT (forecastWorkflow execute requests spot model)

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
