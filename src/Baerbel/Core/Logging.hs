{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Logging where

import Baerbel.Core.JSON ( field, textField )
import Data.Aeson ( Value(..), object, (.=) )
import Data.Aeson.Types ( Pair )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime, formatTime, defaultTimeLocale )
import qualified Data.Aeson.Key as Key ( fromText )
import qualified Data.Vector as Vector ( toList )

eventRecord :: UTCTime -> Text -> [Pair] -> Value
eventRecord now name fields = object (["timestamp" .= stamp, "event" .= name] ++ fields)
  where
    stamp :: String
    stamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" now

projectEvent :: Bool -> Value -> [(Text, [Pair])]
projectEvent owner value@(Object _)
  | owner && kind `elem` ["session.input_transcript.delta", "session.output_transcript.delta"] = transcript kind value
  | owner && kind == "phone.note_rejected" = [("owner_note_rejected", selected ["action", "state", "error"] value)]
  | kind == "session.delegation.created" = [("live_delegation", delegation value)]
  | kind == "response.event" = backendEvents owner value
  | Just (name, keys) <- lookup kind syntheticEvents = [(name, selected keys value)]
  | otherwise = []
  where
    kind :: Text
    kind = fromMaybe "" (textField "type" value)
projectEvent _ _ = []

syntheticEvents :: [(Text, (Text, [Text]))]
syntheticEvents =
  [("phone.tool", ("live_tool", ["delegation_id", "response_id", "tool", "tool_call_id", "status", "error", "elapsed_ms", "result_count", "has_more", "repeat_query"])),
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
valueAt key = fromMaybe Null . field key

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
