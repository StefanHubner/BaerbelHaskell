{-# LANGUAGE OverloadedStrings #-}
module Baerbel.NoteRuntime (NoteObserverConfig(..), NoteRuntime(..), withNoteRuntime) where

import Baerbel.JSON (jsonOptions)
import GHC.Generics (Generic)
import Baerbel.Error (Result, failure)
import Baerbel.Notes (Fragment(..), Transcript(..), NoteEffects(..), NoteRules, notes)
import Baerbel.Provider (Execute)
import Baerbel.Recording (Archive, readClip)
import Baerbel.Speech (Resampler, resample, splitFrames, withResampler)
import Baerbel.VAD (VAD, withVAD, isSpeech)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON, object, (.=))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.Foldable (toList)
import Data.Scientific (toRealFloat)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)

data NoteObserverConfig = NoteObserverConfig
  { observerVadMode :: Int, observerFrameBytes :: Int, observerRate :: Int
  , observerProviderRate :: Int, observerGapFrames :: Int, observerHistoryChars :: Int
  } deriving (Eq, Show, Generic)
instance FromJSON NoteObserverConfig where parseJSON = genericParseJSON (jsonOptions 8)

data NoteRuntime = NoteRuntime
  { noteExecute :: Execute, notePending :: IO Bool
  , noteAudio :: BS.ByteString -> IO (), noteEvent :: Value -> IO ()
  }

data Observation = Observation
  { observedFragments :: Seq Fragment, observedChars :: Int, observedSequence :: Int
  , observedLastDelta :: Double, observedVoiceStart :: Double, observedQuiet :: Int
  , observedPending :: BS.ByteString, observedBound :: Bool, observedDroppedThrough :: Int, observedClosed :: Bool
  }

type Delivery = Text -> Text -> BS.ByteString -> IO (Result Value)

withNoteRuntime :: NoteObserverConfig -> NoteRules -> Maybe Archive -> Delivery -> (Value -> IO ()) -> (NoteRuntime -> IO (Result a)) -> IO (Result a)
withNoteRuntime cfg rules archive deliver report action =
  withResampler (observerProviderRate cfg) (observerRate cfg) $ \converter ->
    withVAD (observerVadMode cfg) $ \vad -> createObserver cfg rules archive deliver report converter vad action

createObserver :: NoteObserverConfig -> NoteRules -> Maybe Archive -> Delivery -> (Value -> IO ()) -> Resampler -> VAD -> (NoteRuntime -> IO (Result a)) -> IO (Result a)
createObserver cfg rules archive deliver report converter vad action = do
  let bound = isJust archive
      initial = Observation Seq.empty 0 0 0 0 (observerGapFrames cfg) BS.empty bound 0 False
  state <- newTVarIO initial
  let reset = atomically (modifyTVar' state (\current -> current {observedPending = BS.empty}))
      rejected actionName phase code = report (object ["type" .= ("phone.note_rejected" :: Text),
        "action" .= actionName, "state" .= phase, "error" .= code])
      effects = NoteEffects (snapshot state) (archiveRead archive) deliver reset freshIdentifier rejected
  (execute, pending, close) <- notes rules effects
  let event value = maybe (pure ()) (uncurry (addTranscript cfg state)) (observeEvent value)
      runtime = NoteRuntime execute pending (observeAudio cfg converter vad state) event
  finally (action runtime) (atomically (modifyTVar' state closeObserver) >> close)

archiveRead :: Maybe Archive -> Double -> Double -> IO (Result BS.ByteString)
archiveRead Nothing _ _ = pure (Left (failure "call_recording_unavailable"))
archiveRead (Just archive) begin end = readClip archive begin end

nowSeconds :: IO Double
nowSeconds = (\value -> fromIntegral value / 1000000000) <$> getMonotonicTimeNSec

snapshot :: TVar Observation -> IO Transcript
snapshot state = do
  current <- readTVarIO state
  now <- nowSeconds
  pure (observedTranscript now current)

observedTranscript :: Double -> Observation -> Transcript
observedTranscript now current = Transcript (toList (observedFragments current)) (observedLastDelta current)
  (observedVoiceStart current) now (observedBound current) (observedSequence current) (observedDroppedThrough current)

observeAudio :: NoteObserverConfig -> Resampler -> VAD -> TVar Observation -> BS.ByteString -> IO ()
observeAudio cfg converter vad state pcm = do
  stopped <- observedClosed <$> readTVarIO state
  unless stopped (processAudio cfg converter vad state pcm)

processAudio :: NoteObserverConfig -> Resampler -> VAD -> TVar Observation -> BS.ByteString -> IO ()
processAudio cfg converter vad state pcm = do
  converted <- resample converter pcm False
  case converted of
    Left _ -> atomically (modifyTVar' state (\current -> current { observedBound = False }))
    Right bytes -> do
      frames <- atomically (stateTVar state (audioFrames cfg bytes))
      mapM_ (observeFrame cfg vad state) frames

audioFrames :: NoteObserverConfig -> BS.ByteString -> Observation -> ([BS.ByteString], Observation)
audioFrames cfg bytes current = (frames, current {observedPending = pending})
  where
    complete = observedPending current <> bytes
    (frames, pending) = splitFrames (observerFrameBytes cfg) complete

observeFrame :: NoteObserverConfig -> VAD -> TVar Observation -> BS.ByteString -> IO ()
observeFrame cfg vad state frame = do
  result <- isSpeech vad (observerRate cfg) frame
  now <- nowSeconds
  atomically (modifyTVar' state (speechTransition (observerGapFrames cfg) now result))

speechTransition :: Int -> Double -> Result Bool -> Observation -> Observation
speechTransition _ _ (Left _) current = current { observedBound = False }
speechTransition _ _ (Right False) current = current { observedQuiet = observedQuiet current + 1 }
speechTransition gap now (Right True) current = current { observedQuiet = 0, observedVoiceStart = started }
  where
    started | observedQuiet current >= gap = now
            | otherwise = observedVoiceStart current

observeEvent :: Value -> Maybe (KM.KeyMap Value, Text)
observeEvent (Object fields)
  | KM.lookup "type" fields == Just (String "session.input_transcript.delta") =
      case KM.lookup "delta" fields of
        Just (String text) | not (T.null text) -> Just (fields, text)
        _ -> Nothing
observeEvent _ = Nothing

addTranscript :: NoteObserverConfig -> TVar Observation -> KM.KeyMap Value -> Text -> IO ()
addTranscript cfg state fields text = do
  now <- nowSeconds
  atomically (modifyTVar' state (appendFragment (observerHistoryChars cfg) now fields text))

appendFragment :: Int -> Double -> KM.KeyMap Value -> Text -> Observation -> Observation
appendFragment _ _ _ _ current | observedClosed current = current
appendFragment limit now fields text current = trimHistory limit updated
  where
    sequenceNumber = observedSequence current + 1
    part = Fragment sequenceNumber text (number "archive_start") (number "archive_end")
    updated = current { observedSequence = sequenceNumber, observedLastDelta = now,
                        observedFragments = observedFragments current |> part,
                        observedChars = observedChars current + T.length text }
    number key = case KM.lookup key fields of
      Just (Number value) -> Just (toRealFloat value)
      _ -> Nothing

trimHistory :: Int -> Observation -> Observation
trimHistory limit current
  | observedChars current <= limit = current
  | otherwise = case Seq.viewl (observedFragments current) of
      EmptyL -> current
      part :< rest -> trimHistory limit current
        { observedFragments = rest, observedChars = observedChars current - T.length (fragmentText part),
          observedDroppedThrough = fragmentSequence part }

freshIdentifier :: IO Text
freshIdentifier = textHex <$> UUID.nextRandom
  where textHex = T.filter (/= '-') . UUID.toText

closeObserver :: Observation -> Observation
closeObserver current = current {observedClosed = True, observedPending = BS.empty,
  observedFragments = Seq.empty, observedChars = 0}
