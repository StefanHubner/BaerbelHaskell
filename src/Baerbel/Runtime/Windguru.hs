{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Windguru (module Baerbel.Core.Windguru, windguru) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.HTTP ( Request(requestQuery, requestUrl) )
import Baerbel.Core.ProviderConfig ( WindguruRules(..) )
import Baerbel.Core.Windguru
import Baerbel.Runtime.Cache ( memoize )
import Baerbel.Runtime.Effects
    ( continue, mapResult, within, monotonicSeconds )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Baerbel.Runtime.Provider ( Execute, boundedMap )
import Data.Aeson hiding (Result, pairs)
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import System.Timeout ( timeout )
import qualified Data.Text as Text

type TimedSend = Request -> IO (Result (Received Value))

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

fetchMember :: WindguruRules -> TimedSend -> IO UTCTime -> LocalZone -> Deadline -> UTCTime -> Int -> SelectedPair -> IO (Bool, Either FailedForecast Forecast)
fetchMember rules send clock local deadline start hours selected@(raw, model, _) = case planMember rules selected of
  Left issue -> pure (False, Left issue)
  Right wire -> do
    response <- beforeDeadline deadline (mapResult (decodeReceived "windguru_invalid_response") (send wire))
    now <- clock
    pure (memberResult rules local now raw model start hours response)

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
