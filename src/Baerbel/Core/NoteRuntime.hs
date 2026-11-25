{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.NoteRuntime where

import Baerbel.Core.JSON (jsonOptions, eventDelta, numberField)
import GHC.Generics (Generic)
import Baerbel.Core.Error (Result)
import Baerbel.Core.Notes (Fragment(..), Transcript(..))
import Baerbel.Core.Speech (splitFrames)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

data NoteObserverConfig = NoteObserverConfig
  { observerVadMode :: Int, observerFrameBytes :: Int, observerRate :: Int
  , observerProviderRate :: Int, observerGapFrames :: Int, observerHistoryChars :: Int
  } deriving (Eq, Show, Generic)
instance FromJSON NoteObserverConfig where parseJSON = genericParseJSON (jsonOptions 8)

data Observation = Observation
  { observedFragments :: Seq Fragment, observedChars :: Int, observedSequence :: Int
  , observedLastDelta :: Double, observedVoiceStart :: Double, observedQuiet :: Int
  , observedPending :: BS.ByteString, observedBound :: Bool, observedDroppedThrough :: Int, observedClosed :: Bool
  }

observedTranscript :: Double -> Observation -> Transcript
observedTranscript now current = Transcript (toList (observedFragments current)) (observedLastDelta current)
  (observedVoiceStart current) now (observedBound current) (observedSequence current) (observedDroppedThrough current)

audioFrames :: NoteObserverConfig -> BS.ByteString -> Observation -> ([BS.ByteString], Observation)
audioFrames cfg bytes current = (frames, current {observedPending = pending})
  where
    complete = observedPending current <> bytes
    (frames, pending) = splitFrames (observerFrameBytes cfg) complete

speechTransition :: Int -> Double -> Result Bool -> Observation -> Observation
speechTransition _ _ (Left _) current = current { observedBound = False }
speechTransition _ _ (Right False) current = current { observedQuiet = observedQuiet current + 1 }
speechTransition gap now (Right True) current = current { observedQuiet = 0, observedVoiceStart = started }
  where
    started | observedQuiet current >= gap = now
            | otherwise = observedVoiceStart current

observeEvent :: Value -> Maybe (KM.KeyMap Value, Text)
observeEvent event@(Object fields) = case eventDelta "session.input_transcript.delta" event of
  Just text | not (T.null text) -> Just (fields, text)
  _ -> Nothing
observeEvent _ = Nothing

appendFragment :: Int -> Double -> KM.KeyMap Value -> Text -> Observation -> Observation
appendFragment _ _ _ _ current | observedClosed current = current
appendFragment limit now fields text current = trimHistory limit updated
  where
    sequenceNumber = observedSequence current + 1
    event = Object fields
    begin = numberField "archive_start" event
    end = numberField "archive_end" event
    part = Fragment sequenceNumber text begin end
    updated = current { observedSequence = sequenceNumber, observedLastDelta = now,
                        observedFragments = observedFragments current |> part,
                        observedChars = observedChars current + T.length text }

trimHistory :: Int -> Observation -> Observation
trimHistory limit current
  | observedChars current <= limit = current
  | otherwise = case Seq.viewl (observedFragments current) of
      EmptyL -> current
      part :< rest -> trimHistory limit current
        { observedFragments = rest, observedChars = observedChars current - T.length (fragmentText part),
          observedDroppedThrough = fragmentSequence part }

closeObserver :: Observation -> Observation
closeObserver current = current {observedClosed = True, observedPending = BS.empty,
  observedFragments = Seq.empty, observedChars = 0}
