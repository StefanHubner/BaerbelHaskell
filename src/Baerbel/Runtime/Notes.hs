module Baerbel.Runtime.Notes
  ( Fragment(..), Transcript(..), NoteEffects(..), NoteRules(..)
  , notes, quoteSequence, completionMatches ) where

import Baerbel.Core.Error ( Failure(..), Result )
import Baerbel.Core.Notes
import Baerbel.Runtime.Concurrent ( stateMVar, updateMVar )
import Baerbel.Runtime.Error ( ioResult )
import Baerbel.Runtime.Provider ( Execute )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( Async, asyncWithUnmask, wait )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar, readMVar )
import Control.Exception ( mask, onException )
import Control.Monad ( void )
import Data.Aeson ( Value )
import Data.ByteString ( ByteString )
import Data.Either ( fromRight )
import Data.Foldable ( traverse_ )
import Data.Text ( Text )
import System.Timeout ( timeout )
import qualified Data.ByteString as Bytes
import qualified Baerbel.Runtime.Effects as Effects

data NoteEffects = NoteEffects
  { noteSnapshot :: IO Transcript, noteReadClip :: Double -> Double -> IO (Result ByteString)
  , noteDeliver :: Text -> Text -> ByteString -> IO (Result Value)
  , noteReset :: IO (), noteIdentifier :: IO Text, noteReport :: Text -> Text -> Text -> IO () }
data Cell = Cell { cellState :: NoteState, cellTask :: Maybe (Async Value) }
awaitSettled :: NoteRules -> NoteEffects -> IO (Maybe Transcript)
awaitSettled rules effects = timeout duration loop
  where
    duration = noteWaitSeconds rules * 1000000
    loop = noteSnapshot effects >>= check
    check transcript
      | settled rules transcript = pure transcript
      | otherwise = threadDelay (notePollMicros rules) >> loop

readState :: MVar Cell -> Transcript -> IO NoteState
readState cell transcript = stateMVar cell (synchronizeCell transcript)

synchronizeCell :: Transcript -> Cell -> (Cell, NoteState)
synchronizeCell transcript current = (current {cellState = state}, state)
  where state = synchronize transcript (cellState current)

start :: NoteRules -> NoteEffects -> MVar Cell -> Text -> IO Reply
start rules effects cell phrase = do
  transcript <- noteSnapshot effects
  modifyMVar cell (startSelection rules effects phrase transcript)

startSelection :: NoteRules -> NoteEffects -> Text -> Transcript -> Cell -> IO (Cell, Reply)
startSelection rules effects phrase transcript current = case select "" phrase transcript state of
  Left code -> pure (current {cellState = state}, Rejected code)
  Right chosen -> do
    ident <- noteIdentifier effects
    noteReset effects
    let updated = chosen {noteSelection = fmap (\selection -> selection {selectionId = ident}) (noteSelection chosen)}
    pure (current {cellState = updated}, Completed (selectionResult rules))
  where state = synchronize transcript (cellState current)

cancel :: NoteEffects -> MVar Cell -> IO Reply
cancel effects cell = modifyMVar cell update
  where
    update current = case cancelSelection (cellState current) of
      Left reply -> pure (current, reply)
      Right state -> noteReset effects >> pure (current {cellState = state}, Completed (result "cancelled"))

send :: NoteRules -> NoteEffects -> MVar Cell -> Text -> IO Reply
send rules effects cell phrase = do
  transcript <- noteSnapshot effects
  state <- readState cell transcript
  case sendDecision phrase transcript state of
    Immediate reply -> pure reply
    Settle draft -> awaitDraft rules effects cell draft

awaitDraft :: NoteRules -> NoteEffects -> MVar Cell -> Draft -> IO Reply
awaitDraft rules effects cell draft = do
  transcript <- awaitSettled rules effects
  mask $ \restore -> do
    accepted <- modifyMVar cell (acceptDraft rules effects draft transcript)
    either (pure . Rejected) (restore . waitDelivery cell) accepted

acceptDraft :: NoteRules -> NoteEffects -> Draft -> Maybe Transcript -> Cell -> IO (Cell, Either Text (Async Value))
acceptDraft rules effects draft transcript current = case confirmDraft rules draft transcript state of
  Left code -> pure (current {cellState = state}, Left code)
  Right prepared -> do
    noteReset effects
    worker <- asyncWithUnmask (\unmask -> unmask (deliver rules effects prepared))
    let updated = state {notePhase = Sending, noteSelection = Nothing}
    pure (Cell updated (Just worker), Right worker)
  where state = maybe (cellState current) (`synchronize` cellState current) transcript

waitDelivery :: MVar Cell -> Async Value -> IO Reply
waitDelivery cell worker = do
  receipt <- wait worker `onException` updateMVar cell cacheUnknown
  updateMVar cell (remember receipt)
  pure (Completed receipt)

remember :: Value -> Cell -> Cell
remember receipt current = current {cellState = state {notePhase = phase, noteReceipt = Just receipt}}
  where
    state = cellState current
    phase | sentReceipt receipt = Sent
          | otherwise = Failed

cacheUnknown :: Cell -> Cell
cacheUnknown current = current {cellState = (cellState current) {noteReceipt = Just unknownOutcome}}

deliver :: NoteRules -> NoteEffects -> Prepared -> IO Value
deliver rules effects prepared = do
  let issue = Failure "note_send_outcome_unknown_do_not_retry" True
  completed <- Effects.within issue duration (ioResult "note_send_outcome_unknown_do_not_retry" operation)
  pure (fromRight unknownOutcome completed)
  where
    duration = noteDeliverySeconds rules * 1000000
    operation = do
      clip <- Effects.attempt "call_recording_excerpt_unavailable" (noteReadClip effects (preparedBegin prepared) (preparedEnd prepared))
      either (const (pure excerptUnavailable)) deliverClip clip
    deliverClip audio
      | Bytes.null audio || Bytes.length audio > noteMaximumBytes rules = pure excerptUnavailable
      | otherwise = fmap (either failureValue id) (noteDeliver effects (preparedId prepared) (preparedSpeech prepared) audio)

close :: NoteEffects -> MVar Cell -> IO ()
close effects cell = do
  worker <- stateMVar cell closeCell
  noteReset effects
  traverse_ (void . wait) worker

closeCell :: Cell -> (Cell, Maybe (Async Value))
closeCell current = (current {cellState = updated}, cellTask current)
  where updated = (cellState current) {noteClosed = True, noteSelection = Nothing}

notes :: NoteRules -> NoteEffects -> IO (Execute, IO Bool, IO ())
notes rules effects = do
  cell <- newMVar (Cell (NoteState Idle Nothing Nothing False) Nothing)
  let execute name arguments _ = executeNote rules effects cell name arguments
      pending = pendingNote effects cell
  pure (execute, pending, close effects cell)

pendingNote :: NoteEffects -> MVar Cell -> IO Bool
pendingNote effects cell = do
  transcript <- noteSnapshot effects
  state <- readState cell transcript
  pure (notePhase state `elem` [Recording, Sending, Limit])

executeNote :: NoteRules -> NoteEffects -> MVar Cell -> Text -> Value -> IO (Result Value)
executeNote rules effects cell name arguments = do
  state <- cellState <$> readMVar cell
  reply <- either (pure . Rejected) run (validateRequest rules state name arguments)
  case reply of
    Completed value -> pure (Right value)
    Rejected code -> do
      transcript <- noteSnapshot effects
      current <- readState cell transcript
      let (action, phase, value) = finishReply rules name code transcript current
      noteReport effects action phase code
      pure (Right value)
  where
    run (Start phrase) = start rules effects cell phrase
    run (Send phrase) = send rules effects cell phrase
    run Cancel = cancel effects cell
