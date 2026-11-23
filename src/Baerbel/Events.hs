module Baerbel.Events (EventRules(..), Event(..), parseEvent, withEvents, pollEvents) where

import Baerbel.Error (Result, failure)
import Baerbel.JSON (jsonOptions)
import Baerbel.Logging (Report)
import Baerbel.Incoming (callbackNumber)
import Baerbel.Messages (Messages(..))
import Baerbel.SMS (Receipt(..))
import Baerbel.Storage (databaseResult, withDatabase)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (bracket)
import Control.Monad (forever, join)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Csv as CSV
import Data.Char (isAscii, isAlphaNum, isDigit, isUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Database.SQLite.Simple
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.IO (Handle, SeekMode(..), hSeek, hTell, hClose)
import qualified System.Posix.Files
import qualified System.Posix.IO
import qualified System.Posix.Types
import System.Posix.Files (getFdStatus, isRegularFile, deviceID, fileID, fileSize)
import System.Posix.IO (OpenMode(..), OpenFileFlags(..), defaultFileFlags, openFd, closeFd, fdToHandle)
import Text.Read (readMaybe)

data EventRules = EventRules
  { eventMaximumLine :: Int, eventMaximumBatch :: Int, eventRotations :: Int
  , eventAnchorBytes :: Int, eventPollMicros :: Int, eventReasons :: [Text]
  , eventIdentifierChars :: Int, eventSuffixChars :: Int, eventReasonChars :: Int, eventMaximumTimestamp :: Integer } deriving (Generic)
instance FromJSON EventRules where parseJSON = genericParseJSON (jsonOptions 5)
data Event = Event
  { eventIdentifier :: Text, eventReason :: Text, eventCallback :: Maybe Text, eventAnswered :: Bool }
  deriving (Eq, Show)
data Cursor = Cursor {cursorKey :: Text, cursorPosition :: Integer, cursorLength :: Int, cursorAnchor :: Text}
data Row = Row (Maybe Event) Cursor

type Fields = (Text, Text, Text, Text, Text, Text, Text)

parseEvent :: EventRules -> ByteString -> Maybe Event
parseEvent rules raw
  | Bytes.length raw > eventMaximumLine rules || not ("\n" `Bytes.isSuffixOf` raw) = Nothing
  | otherwise = either (const Nothing) one decoded
  where
  decoded = CSV.decode CSV.NoHeader (Lazy.fromStrict raw) :: Either String (Vector.Vector Fields)
  one values = case Vector.toList values of
    [fields] -> validateEvent rules fields
    _ -> Nothing

validateEvent :: EventRules -> Fields -> Maybe Event
validateEvent rules (schema, ident, ended, privacy, number, reason, answered)
  | schema /= "incoming-v1" || not (validIdentifier rules ident) || not validTime || not validReason || answered `notElem` ["0", "1"] = Nothing
  | otherwise = Just (Event ident selected (callbackNumber privacy number) (answered == "1"))
  where
  instant = readMaybe (Text.unpack ended) :: Maybe Integer
  validTime = not (Text.null ended) && Text.all (\char -> isAscii char && isDigit char) ended && maybe False (\value -> value > 0 && value < eventMaximumTimestamp rules) instant
  validReason = not (Text.null reason) && Text.length reason <= eventReasonChars rules && Text.all (\char -> isAscii char && isUpper char || char == '_') reason
  selected = if reason `elem` eventReasons rules then reason else "UNKNOWN"

validIdentifier :: EventRules -> Text -> Bool
validIdentifier rules ident = case Text.splitOn "." ident of
  [first] -> part (eventIdentifierChars rules) first
  [first, second] -> part (eventIdentifierChars rules) first && part (eventSuffixChars rules) second
  _ -> False
  where part bound value = not (Text.null value) && Text.length value <= bound && Text.all (\char -> isAscii char && isAlphaNum char || char == '_' || char == '-') value

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
pollEvents rules output messages source connection = fmap join (databaseResult "incoming_events_unavailable" (visit 0 paths))
  where
  paths = [source ++ "." ++ show number | number <- [eventRotations rules, eventRotations rules - 1 .. 1]] ++ [source]
  visit count [] = pure (Right count)
  visit count (path : rest) = do
    exists <- doesFileExist path
    batch <- if exists then readBatch rules connection path else pure (Right [])
    either (pure . Left) (consumeBatch count rest) batch
  consumeBatch count remaining rows = do
    result <- consumeRows output messages connection count rows
    case result of
      Left err -> pure (Left err)
      Right (done, True) -> visit done remaining
      Right (done, False) -> pure (Right done)

readBatch :: EventRules -> Connection -> FilePath -> IO (Result [Row])
readBatch rules connection path = do
  result <- databaseResult "incoming_events_unavailable" (tryIOError opened)
  pure (result >>= either unavailable id)
  where
  unavailable err = if isDoesNotExistError err then Right [] else Left (failure "incoming_events_unavailable")
  flags = defaultFileFlags {nofollow = True, nonBlock = True, cloexec = True}
  opened = bracket (openFd path ReadOnly flags) closeFd inspect
  inspect descriptor = do
    metadata <- getFdStatus descriptor
    if isRegularFile metadata then readSource rules connection descriptor metadata else pure (Left (failure "incoming_events_invalid_source"))

readSource :: EventRules -> Connection -> System.Posix.Types.Fd -> System.Posix.Files.FileStatus -> IO (Result [Row])
readSource rules connection descriptor metadata = do
  rows <- query connection "SELECT position, anchor_length, anchor FROM incoming_event_cursors WHERE file_key=?" (Only key)
  copied <- System.Posix.IO.dup descriptor
  bracket (fdToHandle copied) hClose (readFrom rows)
  where
  key = Text.pack (show (deviceID metadata)) <> ":" <> Text.pack (show (fileID metadata))
  readFrom rows handle = do
    position <- resumePosition handle (fromIntegral (fileSize metadata)) rows
    hSeek handle AbsoluteSeek position
    readRows rules key handle (eventMaximumBatch rules) []

resumePosition :: Handle -> Integer -> [(Integer, Int, Text)] -> IO Integer
resumePosition handle size ((position, count, saved) : _) = do
  prefixBytes <- Bytes.hGet handle count
  pure (if size >= position && digest prefixBytes == saved then position else 0)
resumePosition _ _ [] = pure 0

digest :: ByteString -> Text
digest bytes = Text.pack (show (hash bytes :: Digest SHA256))

readRows :: EventRules -> Text -> Handle -> Int -> [Row] -> IO (Result [Row])
readRows _ _ _ 0 rows = pure (Right (reverse rows))
readRows rules key handle remaining rows = do
  start <- hTell handle
  let maximumRead = eventMaximumLine rules + 1
  bytes <- Bytes.hGet handle maximumRead
  case Bytes.elemIndex 10 bytes of
    Just offset -> recordRow rules key handle remaining rows start (Bytes.take (offset + 1) bytes)
    Nothing | Bytes.length bytes <= eventMaximumLine rules -> pure (Right (reverse rows))
    _ -> pure (Left (failure "incoming_event_record_too_large"))

recordRow :: EventRules -> Text -> Handle -> Int -> [Row] -> Integer -> ByteString -> IO (Result [Row])
recordRow rules key handle remaining rows start raw = do
  let next = start + fromIntegral (Bytes.length raw)
      count = fromIntegral (min next (fromIntegral (eventAnchorBytes rules)))
  hSeek handle AbsoluteSeek 0
  anchor <- digest <$> Bytes.hGet handle count
  hSeek handle AbsoluteSeek next
  let row = Row (parseEvent rules raw) (Cursor key next count anchor)
      rest = remaining - 1
  readRows rules key handle rest (row : rows)

consumeRows :: Report -> Messages -> Connection -> Int -> [Row] -> IO (Result (Int, Bool))
consumeRows _ _ _ count [] = pure (Right (count, True))
consumeRows output messages connection count (Row event cursor : rows) = do
  receipt <- maybe invalid (fmap Just . deliver) event
  if maybe False ((== Just True) . receiptRetryable) receipt then pure (Right (count, False)) else advance
  where
  invalid = output "incoming_event_invalid" [] >> pure Nothing
  deliver value = deliverOutcome messages (eventIdentifier value) (eventReason value) (eventCallback value) (eventAnswered value)
  advance = do
    saved <- databaseResult "incoming_cursor_unavailable" (checkpoint connection cursor)
    let next = count + maybe 0 (const 1) event
    either (pure . Left) (const (consumeRows output messages connection next rows)) saved

checkpoint :: Connection -> Cursor -> IO ()
checkpoint connection cursor = execute connection "INSERT OR REPLACE INTO incoming_event_cursors VALUES (?, ?, ?, ?)"
  (cursorKey cursor, cursorPosition cursor, cursorLength cursor, cursorAnchor cursor)
