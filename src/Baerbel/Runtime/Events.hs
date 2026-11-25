module Baerbel.Runtime.Events (EventRules(..), Event(..), parseEvent, withEvents, pollEvents) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.Events
import Baerbel.Runtime.Logging ( Report )
import Baerbel.Runtime.Messages ( Messages(..) )
import Baerbel.Runtime.SMS ( Receipt(..) )
import Baerbel.Runtime.Storage ( databaseResult, withDatabase )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( withAsync )
import Control.Exception ( bracket )
import Control.Monad ( forever )
import Data.Text ( Text )
import Database.SQLite.Simple
    ( Only(Only), execute, execute_, query, Connection )
import System.Directory ( doesFileExist )
import System.IO ( Handle, SeekMode(..), hSeek, hTell, hClose )
import System.IO.Error ( isDoesNotExistError, tryIOError )
import System.Posix.Files
    ( getFdStatus, isRegularFile, deviceID, fileID, fileSize )
import System.Posix.IO
    ( OpenMode(..),
      OpenFileFlags(..),
      defaultFileFlags,
      openFd,
      closeFd,
      fdToHandle )
import qualified System.Posix.Files
import qualified System.Posix.IO
import qualified System.Posix.Types
import qualified Data.ByteString as Bytes
import qualified Baerbel.Runtime.Effects as Effects
import qualified Data.Text as Text

withEvents :: EventRules -> Report -> Messages -> FilePath -> FilePath -> IO (Result a) -> IO (Result a)
withEvents rules output messages source database action = withDatabase "incoming_cursor_unavailable" database opened
  where
  opened connection = do
    execute_ connection "CREATE TABLE IF NOT EXISTS incoming_event_cursors (file_key TEXT PRIMARY KEY, position INTEGER NOT NULL, anchor_length INTEGER NOT NULL, anchor TEXT NOT NULL)"
    withAsync (forever (poll connection >> threadDelay (eventPollMicros rules))) (const action)
  poll connection = do
    result <- pollEvents rules output messages source connection
    either (const (output "incoming_events_unavailable" [])) (const (pure ())) result

pollEvents :: EventRules -> Report -> Messages -> FilePath -> Connection -> IO (Result Int)
pollEvents rules output messages source connection = Effects.mapResult id (databaseResult "incoming_events_unavailable" (visit 0 paths))
  where
  paths = [source ++ "." ++ show number | number <- [eventRotations rules, eventRotations rules - 1 .. 1]] ++ [source]
  visit count [] = pure (Right count)
  visit count (path : rest) = do
    exists <- doesFileExist path
    let readPresent | exists = readBatch rules connection path
                    | otherwise = pure (Right [])
    batch <- readPresent
    Effects.continue (consumeBatch count rest) batch
  consumeBatch count remaining rows = do
    result <- consumeRows output messages connection count rows
    case result of
      Left err -> pure (Left err)
      Right (done, True) -> visit done remaining
      Right (done, False) -> pure (Right done)

readBatch :: EventRules -> Connection -> FilePath -> IO (Result [Row])
readBatch rules connection path = Effects.mapResult (either unavailable id) operation
  where
  operation = databaseResult "incoming_events_unavailable" (tryIOError opened)
  unavailable err
    | isDoesNotExistError err = Right []
    | otherwise = Left (failure "incoming_events_unavailable")
  flags = defaultFileFlags {nofollow = True, nonBlock = True, cloexec = True}
  opened = bracket (openFd path ReadOnly flags) closeFd inspect
  inspect descriptor = getFdStatus descriptor >>= readRegular descriptor
  readRegular descriptor metadata
    | isRegularFile metadata = readSource rules connection descriptor metadata
    | otherwise = pure (Left (failure "incoming_events_invalid_source"))

readSource :: EventRules -> Connection -> System.Posix.Types.Fd -> System.Posix.Files.FileStatus -> IO (Result [Row])
readSource rules connection descriptor metadata = do
  rows <- query connection "SELECT position, anchor_length, anchor FROM incoming_event_cursors WHERE file_key=?" (Only key)
  copied <- System.Posix.IO.dup descriptor
  bracket (fdToHandle copied) hClose (readFrom rows)
  where
  key = Text.pack (show (deviceID metadata)) <> ":" <> Text.pack (show (fileID metadata))
  readFrom rows handle = do
    let size = fromIntegral (fileSize metadata)
    position <- readPosition handle size rows
    hSeek handle AbsoluteSeek position
    readRows rules key handle (eventMaximumBatch rules) []

readPosition :: Handle -> Integer -> [(Integer, Int, Text)] -> IO Integer
readPosition handle size ((position, count, saved) : _) = do
  prefixBytes <- Bytes.hGet handle count
  let actual = digest prefixBytes
  pure (resumePosition size position saved actual)
readPosition _ _ [] = pure 0

readRows :: EventRules -> Text -> Handle -> Int -> [Row] -> IO (Result [Row])
readRows _ _ _ 0 rows = pure (Right (reverse rows))
readRows rules key handle remaining rows = do
  start <- hTell handle
  let maximumRead = eventMaximumLine rules + 1
  bytes <- Bytes.hGet handle maximumRead
  case Bytes.elemIndex 10 bytes of
    Just offset -> do
      let width = offset + 1
          raw = Bytes.take width bytes
          next = start + fromIntegral width
          count = fromIntegral (min next (fromIntegral (eventAnchorBytes rules)))
      hSeek handle AbsoluteSeek 0
      anchor <- Bytes.hGet handle count
      hSeek handle AbsoluteSeek next
      let row = recordRow rules key next count raw anchor
          rest = remaining - 1
      readRows rules key handle rest (row : rows)
    Nothing | Bytes.length bytes <= eventMaximumLine rules -> pure (Right (reverse rows))
    _ -> pure (Left (failure "incoming_event_record_too_large"))

consumeRows :: Report -> Messages -> Connection -> Int -> [Row] -> IO (Result (Int, Bool))
consumeRows _ _ _ count [] = pure (Right (count, True))
consumeRows output messages connection count (Row event cursor : rows) = do
  receipt <- maybe invalid (fmap Just . deliver) event
  case receipt >>= receiptRetryable of
    Just True -> pure (Right (count, False))
    _ -> advance
  where
  invalid = output "incoming_event_invalid" [] >> pure Nothing
  deliver value = deliverOutcome messages (eventIdentifier value) (eventReason value) (eventCallback value) (eventAnswered value)
  advance = do
    saved <- databaseResult "incoming_cursor_unavailable" (checkpoint connection cursor)
    let next = count + maybe 0 (const 1) event
    Effects.continue (const (consumeRows output messages connection next rows)) saved

checkpoint :: Connection -> Cursor -> IO ()
checkpoint connection cursor = execute connection "INSERT OR REPLACE INTO incoming_event_cursors VALUES (?, ?, ?, ?)"
  (cursorKey cursor, cursorPosition cursor, cursorLength cursor, cursorAnchor cursor)
