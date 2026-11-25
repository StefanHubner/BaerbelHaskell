{-# LANGUAGE OverloadedStrings #-}
module Baerbel.ManagedMedia
  ( Registry, ManagedCall(..), newRegistry, withManaged
  ) where

import Baerbel.AudioSocket (SocketRules, boundedWritePacket, Packet(..), PacketKind(..), readPacket)
import Baerbel.Error (Failure, Result, failure)
import Baerbel.AudioMetrics (AudioDelivery(..))
import Baerbel.Live (LiveControls, Media(..))
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.STM
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Socket)
import System.Timeout (timeout)

data Attachment = Attachment { attachmentSocket :: Socket, attachmentSerial :: Int }
data PacketAction = ReceiveNext | AwaitResume Int | DeliverAudio (Maybe ByteString) | DeliverDigit ByteString
data MediaState = MediaState
  { activeAttachment :: Attachment, statePaused :: Bool, mediaEnded :: Bool, stateEpoch :: Int
  , stateDeadline :: Maybe Word64, stateFailure :: Maybe Failure }
data Entry = Entry
  { entryState :: TVar MediaState
  , entryControls :: TVar (Maybe LiveControls)
  , entryKeypad :: TVar (ByteString -> IO ())
  , entryReattachAllowed :: TVar (IO Bool), entryPauseMicros :: Int, entrySocketRules :: SocketRules }
data Registry = Registry SocketRules Int (TVar (M.Map Text Entry))

data ManagedCall = ManagedCall
  { managedMedia :: Media, pauseManaged :: IO (), resumeManaged :: IO ()
  , waitManaged :: Int -> IO (Result ()), managedControls :: IO (Maybe LiveControls)
  , managedPaused :: IO Bool, managedEpoch :: IO Int
  , setKeypad :: (ByteString -> IO ()) -> IO ()
  , setReattachAllowed :: IO Bool -> IO () }

newRegistry :: SocketRules -> Int -> IO Registry
newRegistry rules pauseMicros = Registry rules pauseMicros <$> newTVarIO M.empty

newEntry :: SocketRules -> Int -> Socket -> STM Entry
newEntry rules pauseMicros connection = Entry <$> newTVar initial <*> newTVar Nothing <*> newTVar (const (pure ())) <*> newTVar (pure True) <*> pure pauseMicros <*> pure rules
  where
    initial :: MediaState
    initial = MediaState (Attachment connection 0) False False 0 Nothing Nothing

register :: Registry -> Text -> Socket -> IO (Result (Entry, Bool, Int))
register (Registry rules pauseMicros registry) identifier connection = do
  calls <- readTVarIO registry
  maybe (atomically (insertEntry rules pauseMicros registry identifier connection)) reuse (M.lookup identifier calls)
  where
    reuse :: Entry -> IO (Result (Entry, Bool, Int))
    reuse entry = do
      permitted <- join (readTVarIO (entryReattachAllowed entry))
      now <- getMonotonicTimeNSec
      let attach | permitted = atomically (reattach now entry connection)
                 | otherwise = pure (Left (failure "call_has_left_assistant"))
      attach

insertEntry :: SocketRules -> Int -> TVar (M.Map Text Entry) -> Text -> Socket -> STM (Result (Entry, Bool, Int))
insertEntry rules pauseMicros registry identifier connection = do
  calls <- readTVar registry
  let insert | M.member identifier calls = pure (Left (failure "call_already_attached"))
             | otherwise = create calls
  insert
  where
    create :: M.Map Text Entry -> STM (Result (Entry, Bool, Int))
    create calls = do
      entry <- newEntry rules pauseMicros connection
      writeTVar registry (M.insert identifier entry calls)
      pure (Right (entry, True, 0))

reattach :: Word64 -> Entry -> Socket -> STM (Result (Entry, Bool, Int))
reattach now entry connection = do
  state <- readTVar (entryState entry)
  case reattached now connection state of
    Left err -> pure (Left err)
    Right next -> do
      writeTVar (entryState entry) next
      pure (Right (entry, False, attachmentSerial (activeAttachment next)))

reattached :: Word64 -> Socket -> MediaState -> Result MediaState
reattached now connection state
  | maybe False (now <) (stateDeadline state) && statePaused state && not (mediaEnded state) = Right next
  | otherwise = Left (failure "call_already_attached")
  where
    serial = attachmentSerial (activeAttachment state) + 1
    next = state {activeAttachment = Attachment connection serial, statePaused = False,
      stateEpoch = stateEpoch state + 1, stateDeadline = Nothing}

withManaged :: Registry -> Text -> Socket -> (ManagedCall -> IO (Result ())) -> IO (Result ())
withManaged registry identifier connection action = do
  result <- register registry identifier connection
  case result of
    Left problem -> pure (Left problem)
    Right (entry, True, _) -> withAsync (watchPause entry) (const (action (managed entry))) `finally` unregister registry identifier entry
    Right (entry, False, serial) -> atomically (waitDetached entry serial) >> pure (Right ())

unregister :: Registry -> Text -> Entry -> IO ()
unregister (Registry _ _ registry) identifier entry = atomically $ do
  modifyTVar' (entryState entry) (\state -> state {mediaEnded = True})
  modifyTVar' registry (M.delete identifier)

waitDetached :: Entry -> Int -> STM ()
waitDetached entry serial = do
  state <- readTVar (entryState entry)
  check (mediaEnded state || attachmentSerial (activeAttachment state) /= serial)

managed :: Entry -> ManagedCall
managed entry = ManagedCall (media entry) (setPaused entry True) (setPaused entry False) (waitResume entry) controls paused epoch keypad allow
  where
    paused :: IO Bool
    paused = statePaused <$> readTVarIO (entryState entry)
    epoch :: IO Int
    epoch = stateEpoch <$> readTVarIO (entryState entry)
    controls :: IO (Maybe LiveControls)
    controls = readTVarIO (entryControls entry)
    allow :: IO Bool -> IO ()
    allow checkAllowed = atomically (writeTVar (entryReattachAllowed entry) checkAllowed)
    keypad :: (ByteString -> IO ()) -> IO ()
    keypad callback = atomically (writeTVar (entryKeypad entry) callback)

media :: Entry -> Media
media entry = Media
  { receiveAudio = receive entry
  , sendAudio = send entry
  , finishCall = finish entry
  , observeEvent = const (pure ())
  , observeInputAudio = const (pure ())
  , mediaPaused = statePaused <$> readTVarIO (entryState entry)
  , mediaEpoch = stateEpoch <$> readTVarIO (entryState entry)
  , muteInput = pure False
  , progressSuppressed = pure False
  , beforeTool = \_ _ -> pure (Right ())
  , afterTool = \_ _ -> pure ()
  , installControls = atomically . writeTVar (entryControls entry) . Just
  }

setPaused :: Entry -> Bool -> IO ()
setPaused entry paused = do
  now <- getMonotonicTimeNSec
  atomically (modifyTVar' (entryState entry) (pauseState (entryPauseMicros entry) now paused))

pauseState :: Int -> Word64 -> Bool -> MediaState -> MediaState
pauseState duration now paused state = state
  {statePaused = paused, stateEpoch = stateEpoch state + 1, stateDeadline = deadline}
  where
    deadline | paused = Just (now + fromIntegral duration * 1000)
             | otherwise = Nothing

watchPause :: Entry -> IO ()
watchPause entry = do
  current <- atomically (awaitPaused entry)
  maybe (pure ()) (watchDeadline entry) current

awaitPaused :: Entry -> STM (Maybe MediaState)
awaitPaused entry = do
  state <- readTVar (entryState entry)
  check (mediaEnded state || statePaused state)
  let active | mediaEnded state = Nothing
             | otherwise = Just state
  pure active

watchDeadline :: Entry -> MediaState -> IO ()
watchDeadline entry state = do
  now <- getMonotonicTimeNSec
  let deadline = fromMaybe now (stateDeadline state)
      micros | deadline <= now = 0
             | otherwise = fromIntegral ((deadline - now + 999) `div` 1000)
  changed <- timeout micros (atomically (awaitEpoch entry (stateEpoch state)))
  let expire = atomically (modifyTVar' (entryState entry) (expirePause state))
  maybe expire (const (pure ())) changed
  watchPause entry

awaitEpoch :: Entry -> Int -> STM ()
awaitEpoch entry epoch = do
  state <- readTVar (entryState entry)
  check (mediaEnded state || stateEpoch state /= epoch)

expirePause :: MediaState -> MediaState -> MediaState
expirePause previous state
  | stateEpoch state == stateEpoch previous && statePaused state = state {mediaEnded = True, stateFailure = Just (failure "call_resume_timeout")}
  | otherwise = state

waitResume :: Entry -> Int -> IO (Result ())
waitResume entry seconds = do
  result <- timeout (seconds * 1000000) (atomically ready)
  pure (fromMaybe (Left (failure "call_resume_timeout")) result)
  where
    ready :: STM (Result ())
    ready = do
      state <- readTVar (entryState entry)
      check (mediaEnded state || not (statePaused state))
      pure (resumeOutcome state)

resumeOutcome :: MediaState -> Result ()
resumeOutcome state = case attached state of
  Left err -> Left err
  Right Nothing -> Left (failure "call_ended")
  Right (Just _) -> Right ()

attached :: MediaState -> Result (Maybe Attachment)
attached state
  | Just err <- stateFailure state = Left err
  | mediaEnded state = Right Nothing
  | otherwise = Right (Just (activeAttachment state))

receive :: Entry -> IO (Result (Maybe ByteString))
receive entry = do
  attachment <- attached <$> readTVarIO (entryState entry)
  either (pure . Left) (maybe (pure (Right Nothing)) (receiveFrom entry)) attachment

receiveFrom :: Entry -> Attachment -> IO (Result (Maybe ByteString))
receiveFrom entry attachment = do
  state <- readTVarIO (entryState entry)
  packet <- race (readPacket (attachmentSocket attachment)) (atomically (waitDetached entry (attachmentSerial attachment)))
  latest <- readTVarIO (entryState entry)
  let action
        | mediaEnded latest || attachmentSerial (activeAttachment latest) /= attachmentSerial attachment = Right ReceiveNext
        | otherwise = either (readPaused state latest) (const (Right ReceiveNext)) packet
  either (pure . Left) perform action
  where
    perform ReceiveNext = receive entry
    perform (AwaitResume epoch) = atomically (awaitEpoch entry epoch) >> receive entry
    perform (DeliverAudio audio) = pure (Right audio)
    perform (DeliverDigit digit) = do
      callback <- readTVarIO (entryKeypad entry)
      callback digit
      receive entry

readPaused :: MediaState -> MediaState -> Result Packet -> Result PacketAction
readPaused previous latest packet
  | statePaused latest && disconnected packet = Right (AwaitResume (stateEpoch latest))
  | statePaused latest || stateEpoch previous /= stateEpoch latest = Right ReceiveNext
  | otherwise = handlePacket packet

disconnected :: Result Packet -> Bool
disconnected (Left _) = True
disconnected (Right (Packet Hangup _)) = True
disconnected _ = False

handlePacket :: Result Packet -> Result PacketAction
handlePacket (Left problem)
  | problem == failure "audiosocket_incomplete_read" = Right (DeliverAudio Nothing)
  | otherwise = Left problem
handlePacket (Right (Packet Hangup _)) = Right (DeliverAudio Nothing)
handlePacket (Right (Packet Audio pcm)) = Right (DeliverAudio (Just pcm))
handlePacket (Right (Packet Dtmf digit)) = Right (DeliverDigit digit)
handlePacket (Right _) = Left (failure "audiosocket_unexpected_packet")

send :: Entry -> ByteString -> IO (Result AudioDelivery)
send entry pcm = do
  state <- readTVarIO (entryState entry)
  let deliver | mediaEnded state || statePaused state = pure (Right Suppressed)
              | otherwise = sendCurrent entry state pcm
  deliver

finish :: Entry -> IO (Result ())
finish entry = do
  state <- atomically (markEnded entry)
  boundedWritePacket (entrySocketRules entry) (attachmentSocket (activeAttachment state)) (Packet Hangup "")

markEnded :: Entry -> STM MediaState
markEnded entry = do
  state <- readTVar (entryState entry)
  writeTVar (entryState entry) state {mediaEnded = True}
  pure state

sendCurrent :: Entry -> MediaState -> ByteString -> IO (Result AudioDelivery)
sendCurrent entry state pcm = do
  outcome <- race (boundedWritePacket (entrySocketRules entry) connection (Packet Audio pcm)) (atomically changed)
  pure (either (fmap (const Delivered)) (const (Right Suppressed)) outcome)
  where
    connection :: Socket
    connection = attachmentSocket (activeAttachment state)
    changed :: STM ()
    changed = do
      latest <- readTVar (entryState entry)
      check (mediaEnded latest || stateEpoch latest /= stateEpoch state)
