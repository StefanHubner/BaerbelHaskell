{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Logging
  ( Report, withLogging, eventRecord, reportOwnerEvent, reportIncomingEvent, projectEvent
  ) where

import Baerbel.Error (Failure(..), Result, failure, ioResult)
import Baerbel.JSON (encodeBytes)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Control.Exception (IOException, bracket, bracketOnError, try)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import qualified Data.Vector as Vector
import System.IO (Handle, hClose, hFlush, stdout, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getFdStatus, getFileStatus, deviceID, fileID, FileStatus)
import System.Posix.IO (OpenMode(WriteOnly), OpenFileFlags(..), defaultFileFlags, openFd, closeFd, fdToHandle)
import System.Posix.Types (DeviceID, FileID, Fd)

type Report = Text -> [Pair] -> IO ()
type Identity = (DeviceID, FileID)
data Watched = Watched {watchedHandle :: Handle, watchedIdentity :: Identity}

withLogging :: Maybe FilePath -> (Report -> IO (Result a)) -> IO (Result a)
withLogging path action = do
  opened <- traverse openLog path
  case sequence opened of
    Left problem -> pure (Left problem)
    Right watched -> bracket (newMVar watched) closeLog (action . report path)

openLog :: FilePath -> IO (Result Watched)
openLog path = ioResult "log_file_unavailable" (openWatched path)

openWatched :: FilePath -> IO Watched
openWatched path = bracketOnError (openFd path WriteOnly flags) closeFd convert
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags {append = True, creat = Just 0o666, cloexec = True}
    convert :: Fd -> IO Watched
    convert descriptor = do
      metadata <- getFdStatus descriptor
      handle <- fdToHandle descriptor
      pure (Watched handle (identity metadata))

identity :: FileStatus -> Identity
identity metadata = (deviceID metadata, fileID metadata)

closeLog :: MVar (Maybe Watched) -> IO ()
closeLog state = readMVar state >>= maybe (pure ()) closeWatched

closeWatched :: Watched -> IO ()
closeWatched watched = void (ioResult "log_close_failed" (hClose (watchedHandle watched)))

report :: Maybe FilePath -> MVar (Maybe Watched) -> Report
report path state name fields = modifyMVar_ state emit
  where
    emit :: Maybe Watched -> IO (Maybe Watched)
    emit watched = do
      now <- getCurrentTime
      let bytes = encodeBytes (eventRecord now name fields) <> "\n"
      writeLog stdout bytes
      writeWatched path watched bytes

eventRecord :: UTCTime -> Text -> [Pair] -> Value
eventRecord now name fields = object (["timestamp" .= stamp, "event" .= name] ++ fields)
  where
    stamp :: String
    stamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" now

writeLog :: Handle -> BS.ByteString -> IO ()
writeLog handle bytes = do
  result <- ioResult "log_write_failed" (BS.hPut handle bytes >> hFlush handle)
  either reportFailure (const (pure ())) result

reportFailure :: Failure -> IO ()
reportFailure problem = void (ioResult "log_diagnostic_failed" (Text.hPutStrLn stderr (failureCode problem)))

writeWatched :: Maybe FilePath -> Maybe Watched -> BS.ByteString -> IO (Maybe Watched)
writeWatched Nothing watched _ = pure watched
writeWatched (Just path) watched bytes = do
  refreshed <- refreshFile path watched
  case refreshed of
    Left problem -> reportFailure problem >> pure watched
    Right current -> writeLog (watchedHandle current) bytes >> pure (Just current)

refreshFile :: FilePath -> Maybe Watched -> IO (Result Watched)
refreshFile path Nothing = openLog path
refreshFile path (Just watched) = do
  current <- try (getFileStatus path)
  case observedIdentity current of
    Left problem -> pure (Left problem)
    Right (Just same) | same == watchedIdentity watched -> pure (Right watched)
    Right _ -> replace
  where
    replace :: IO (Result Watched)
    replace = do
      opened <- openLog path
      either (const (pure ())) (const (closeWatched watched)) opened
      pure opened

observedIdentity :: Either IOException FileStatus -> Result (Maybe Identity)
observedIdentity (Right metadata) = Right (Just (identity metadata))
observedIdentity (Left problem)
  | isDoesNotExistError problem = Right Nothing
  | otherwise = Left (failure "log_file_unavailable")

reportOwnerEvent :: Report -> Text -> Value -> IO ()
reportOwnerEvent = reportProjected True

reportIncomingEvent :: Report -> Text -> Value -> IO ()
reportIncomingEvent = reportProjected False

reportProjected :: Bool -> Report -> Text -> Value -> IO ()
reportProjected owner output identifier event = mapM_ emit (projectEvent owner event)
  where
    emit :: (Text, [Pair]) -> IO ()
    emit (name, fields) = output name (("call_id" .= identifier) : fields)

projectEvent :: Bool -> Value -> [(Text, [Pair])]
projectEvent owner value@(Object fields)
  | owner && kind `elem` ["session.input_transcript.delta", "session.output_transcript.delta"] = transcript kind value
  | owner && kind == "phone.note_rejected" = [("owner_note_rejected", selected ["action", "state", "error"] value)]
  | kind == "session.delegation.created" = [("live_delegation", delegation value)]
  | kind == "response.event" = backendEvents owner value
  | Just (name, keys) <- lookup kind syntheticEvents = [(name, selected keys value)]
  | otherwise = []
  where
    kind :: Text
    kind = case KM.lookup "type" fields of {Just (String text) -> text; _ -> ""}
projectEvent _ _ = []

syntheticEvents :: [(Text, (Text, [Text]))]
syntheticEvents =
  [("phone.tool", ("live_tool", ["delegation_id", "response_id", "tool", "tool_call_id", "status", "elapsed_ms", "result_count", "has_more", "repeat_query"])),
   ("phone.session_started", ("live_session_started", ["role", "setup_ms", "model", "voice", "backend_model", "web_search_enabled"])),
   ("phone.session_closed", ("live_session_closed", ["finalized", "final_usage", "reason", "duration_ms"])),
   ("phone.audio_started", ("live_audio_started", ["first_audio_ms"])),
   ("phone.speech_started", ("live_speech_started", ["first_non_silent_audio_ms"])),
   ("phone.call_summary", ("live_call_summary", ["input_audio_ms", "output_audio_ms", "max_output_queue_ms", "first_audio_ms", "first_non_silent_audio_ms"])),
   ("phone.provider_error", ("live_provider_error", ["code", "error_type", "param", "response_id", "delegation_id"])),
   ("phone.background_progress", ("live_background_progress", ["pending_requests"])),
   ("phone.greeting_accepted", ("live_greeting_accepted", ["elapsed_ms"]))]

selected :: [Text] -> Value -> [Pair]
selected keys value = [(Key.fromText key, valueAt key value) | key <- keys]

valueAt :: Text -> Value -> Value
valueAt key (Object fields) = fromMaybe Null (KM.lookup (Key.fromText key) fields)
valueAt _ _ = Null

transcript :: Text -> Value -> [(Text, [Pair])]
transcript kind value = [("live_transcript", ("speaker" .= speaker) : selected ["delta", "start_ms", "end_ms"] value)]
  where
    speaker :: Text
    speaker
      | kind == "session.input_transcript.delta" = "user"
      | otherwise = "assistant"

delegation :: Value -> [Pair]
delegation value = ["delegation_id" .= preferred "id" "delegation_id", "target" .= preferred "target" "target"]
  where
    nested :: Value
    nested = valueAt "delegation" value
    preferred :: Text -> Text -> Value
    preferred key fallback = case valueAt key nested of {Null -> valueAt fallback value; present -> present}

backendEvents :: Bool -> Value -> [(Text, [Pair])]
backendEvents owner envelope = lifecycle ++ search ++ sources
  where
    event :: Value
    event = valueAt "event" envelope
    kind :: Value
    kind = valueAt "type" event
    identifier :: Pair
    identifier = "delegation_id" .= valueAt "delegation_id" envelope
    lifecycle :: [(Text, [Pair])]
    lifecycle = [("live_backend", [identifier, "backend_event_type" .= kind]) | kind `elem` map String ["response.created", "response.completed", "response.failed", "response.incomplete"]]
    item :: Value
    item = valueAt "item" event
    search :: [(Text, [Pair])]
    search = [("live_web_search", identifier : selected ["status"] item ++ ["item_type" .= valueAt "type" item]) |
      kind == String "response.output_item.done", valueAt "type" item == String "web_search_call"]
    sources :: [(Text, [Pair])]
    sources = [("live_source", identifier : selected ["url", "title"] annotation) |
      owner, annotation <- annotations kind event item, valueAt "type" annotation == String "url_citation"]

annotations :: Value -> Value -> Value -> [Value]
annotations (String "response.output_text.annotation.added") event _ = [valueAt "annotation" event]
annotations (String "response.output_item.done") _ item = concatMap (arrayValues . valueAt "annotations") (arrayValues (valueAt "content" item))
annotations _ _ _ = []

arrayValues :: Value -> [Value]
arrayValues (Array values) = Vector.toList values
arrayValues _ = []
