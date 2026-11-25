{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.MapsRoute (module Baerbel.Core.MapsRoute, route) where

import Baerbel.Core.Error ( Result )
import Baerbel.Core.JSON ( decodeValue )
import Baerbel.Core.MapTypes ( Place )
import Baerbel.Core.MapsRoute
import Baerbel.Core.ProviderConfig ( RouteRules(..) )
import Baerbel.Runtime.Effects ( continue )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Data.Aeson hiding (Result, pairs)
import System.Timeout ( timeout )

route :: RouteRules -> Send -> Bool -> Place -> Place -> IO (Result Value)
route rules send elevation origin destination = case routeRequest rules origin destination of
  Left issue -> pure (Left issue)
  Right request -> do
    response <- requestJSON send request (decodeValue "maps_invalid_route_response")
    continue complete (response >>= normalize rules origin destination)
  where
    complete (value, path)
      | not elevation = pure (finishRoute rules value (unavailable "not_enabled", resultSteps value))
      | otherwise = case planTerrain rules path of
        TerrainUnavailable reason -> pure (finishRoute rules value (unavailable reason, pathSteps path))
        TerrainSamples samples positions spacing -> survey value path samples positions spacing
    survey value path samples positions spacing = do
      let request = elevationRequest rules samples
          durationMicros = routeElevationTimeout rules * 1000000
      response <- timeout durationMicros (requestJSON send request (decodeValue "invalid_elevation_response"))
      let ground = terrain rules path samples positions spacing response
      pure (finishRoute rules value ground)
