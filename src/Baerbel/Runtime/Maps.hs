{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Maps (module Baerbel.Core.Maps, maps) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( decodeValue )
import Baerbel.Core.MapTypes ( Place )
import Baerbel.Core.Maps
import Baerbel.Core.ProviderConfig ( MapsRules(..), RouteRules )
import Baerbel.Runtime.Concurrent ( stateMVar )
import Baerbel.Runtime.Effects ( continue )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Baerbel.Runtime.MapsRoute ( route )
import Baerbel.Runtime.Provider ( Execute )
import Control.Concurrent.MVar ( MVar, newMVar, readMVar )
import Crypto.Random ( getRandomBytes )
import Data.Aeson hiding (Result)
import Data.ByteString ( ByteString )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

type Places = MVar MapState

saveCandidate :: MapsRules -> Places -> (Place, Candidate) -> IO Candidate
saveCandidate rules places candidate = do
  bytes <- getRandomBytes (mapsReferenceBytes rules) :: IO ByteString
  stateMVar places (cacheCandidate rules bytes candidate)

lookupArea :: MapsRules -> (Text -> Text) -> Send -> Places -> Text -> IO (Result AreaResolution)
lookupArea rules redact send state text = do
  current <- readMVar state
  case Map.lookup key (fst (storedAreas current)) of
    Just value -> pure (Right (ResolvedArea value))
    Nothing -> do
      let request = geocodeRequest rules text
      response <- requestJSON send request geocode
      continue remember (response >>= resolveArea rules redact)
  where
    key = Text.toCaseFold text
    remember result@(ResolvedArea value) = stateMVar state (cacheArea rules key value result)
    remember result = pure (Right result)

findPlace :: MapsRules -> (Text -> Text) -> Send -> Places -> FindArgs -> IO (Result Value)
findPlace rules redact send places args = case planFind rules args of
  Left issue -> pure (Left issue)
  Right (FindContext result) -> pure (Right (toJSON result))
  Right (FindQuery area query) -> do
    found <- lookupArea rules redact send places area
    continue (searchArea rules redact send places query) found

searchArea :: MapsRules -> (Text -> Text) -> Send -> Places -> Text -> AreaResolution -> IO (Result Value)
searchArea rules _ _ _ _ (AreaChoices candidates) = pure (Right (toJSON result))
  where result = NeedsContext "error" "needs_context" (mapsAreaPrompt rules) (Just candidates)
searchArea rules redact send places query (ResolvedArea area) = do
  let request = searchRequest rules query area
  response <- requestJSON send request (decodeValue "maps_invalid_place_response")
  continue resolve (response >>= placeCandidates rules redact)
  where
    resolve Nothing = do
      let request = geocodeRequest rules query
      fallback <- requestJSON send request geocode
      continue save (fallback >>= geocodeCandidates rules redact)
    resolve (Just candidates) = save candidates
    save = savePlaces rules redact places area

savePlaces :: MapsRules -> (Text -> Text) -> Places -> Geocode -> (Bool, [(Place, Candidate)]) -> IO (Result Value)
savePlaces rules redact places area (more, candidates) = do
  saved <- traverse (saveCandidate rules places) candidates
  pure (Right (toJSON (findResult rules redact area more saved)))

maps :: MapsRules -> RouteRules -> (Text -> Text) -> Send -> Bool -> IO UTCTime -> IO Execute
maps rules routeRules redact send elevation clock = do
  places <- newMVar (MapState (Map.empty, []) (Map.empty, []))
  pure (execute rules routeRules redact (apiSend rules send) elevation clock places)

execute :: MapsRules -> RouteRules -> (Text -> Text) -> Send -> Bool -> IO UTCTime -> Places -> Execute
execute rules routeRules redact send elevation clock places name arguments _ = case name of
  "maps_find_place" -> continue (findPlace rules redact send places) (decodeValue "maps_invalid_arguments" arguments)
  "maps_place_details" -> continue details (decodeValue "maps_invalid_arguments" arguments)
  "maps_route" -> continue directions (decodeValue "maps_invalid_arguments" arguments)
  _ -> pure (Left (failure "maps_unsupported_tool"))
  where
    details args = do
      known <- fmap (fst . storedPlaces) (readMVar places)
      let place = resolvePlace known (detailsPlaceRef args)
      continue (retrieve (detailsPlaceRef args)) place
    retrieve reference place = do
      let request = detailsRequest rules place
      response <- requestJSON send request (decodeValue "maps_invalid_place_response")
      now <- clock
      pure (response >>= placeDetails rules redact now reference place)
    directions args = do
      known <- fmap (fst . storedPlaces) (readMVar places)
      let origin = resolvePlace known (routeOriginRef args)
          destination = resolvePlace known (routeDestinationRef args)
          resolved = (,) <$> origin <*> destination
      continue (uncurry (route routeRules send elevation)) resolved


apiSend :: MapsRules -> Send -> Send
apiSend rules send request = requestJSON send request (apiResponse rules)
