{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.NoteRuntime (NoteObserverConfig(..), NoteRuntime(..), withNoteRuntime) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.NoteRuntime
import Baerbel.Runtime.Notes
    ( NoteRules, Transcript, NoteEffects(NoteEffects), notes )
import Baerbel.Runtime.Provider ( Execute )
import Baerbel.Runtime.Recording ( Archive, readClip )
import Baerbel.Runtime.Speech
    ( Resampler, withResampler, resample )
import Baerbel.Runtime.VAD ( VAD, withVAD, isSpeech )
import Control.Concurrent.STM
    ( atomically, TVar, newTVarIO, readTVarIO, modifyTVar', stateTVar )
import Control.Exception ( finally )
import Control.Monad ( unless )
import Data.Aeson ( object, Value, KeyValue((.=)) )
import Data.Maybe ( isJust )
import Data.Text ( Text )
import qualified Data.ByteString as BS
import qualified Baerbel.Runtime.Effects as Effects
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

data NoteRuntime = NoteRuntime
  { noteExecute :: Execute, notePending :: IO Bool
  , noteAudio :: BS.ByteString -> IO (), noteEvent :: Value -> IO ()
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

snapshot :: TVar Observation -> IO Transcript
snapshot state = do
  current <- readTVarIO state
  now <- Effects.monotonicSeconds
  pure (observedTranscript now current)

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

observeFrame :: NoteObserverConfig -> VAD -> TVar Observation -> BS.ByteString -> IO ()
observeFrame cfg vad state frame = do
  result <- isSpeech vad (observerRate cfg) frame
  now <- Effects.monotonicSeconds
  atomically (modifyTVar' state (speechTransition (observerGapFrames cfg) now result))

addTranscript :: NoteObserverConfig -> TVar Observation -> KM.KeyMap Value -> Text -> IO ()
addTranscript cfg state fields text = do
  now <- Effects.monotonicSeconds
  atomically (modifyTVar' state (appendFragment (observerHistoryChars cfg) now fields text))

freshIdentifier :: IO Text
freshIdentifier = textHex <$> UUID.nextRandom
  where textHex = T.filter (/= '-') . UUID.toText
