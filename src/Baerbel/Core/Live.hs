{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Live where

import Baerbel.Core.JSON (eventDelta, field, textField, numberField)
import Baerbel.Core.Background (BackgroundRules)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.Speech (splitFrames)
import Baerbel.Core.Recording (Timeline, transcriptBounds)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust)
import Data.Aeson (Value(..), decodeStrict', encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

data ToolLimits = ToolLimits
  { maxPendingResponses :: Int, maxResponseCalls :: Int
  , maxToolCalls :: Int, maxToolText :: Int
  } deriving (Eq, Show)

data LiveConfig = LiveConfig
  { liveApiKey :: Text, liveHost :: String, livePort :: Int, livePath :: String
  , liveSession :: Value, liveGreeting :: Maybe Value
  , liveSetupMicros :: Int, liveWriteMicros :: Int
  , liveCloseMicros :: Int, liveStartupCloseMicros :: Int
  , liveFrameBytes :: Int, liveFrameMicros :: Int
  , liveInputRate :: Int, liveProviderRate :: Int
  , liveQueueFrames :: Int, liveStartupFrames :: Int, liveToolLimits :: ToolLimits
  , liveToolMicros :: Int, liveDirectMicros :: Int
  , liveMutationErrors :: Map.Map Text Text, liveBackground :: BackgroundRules
  , liveMessageBytes :: Int, liveErrorFields :: Map.Map Text (Map.Map Text Value)
  } deriving (Eq)

data ToolCall = ToolCall
  { toolCallId :: Text, toolName :: Text, toolArguments :: Text
  } deriving (Eq, Show)

data ToolBatch = ToolBatch
  { delegationId :: Text, responseId :: Text, batchCalls :: [ToolCall]
  } deriving (Eq, Show)

data ToolState = ToolState
  { batches :: Map.Map (Text, Text) ToolBatch
  , activeResponses :: Map.Map Text Text
  , knownCalls :: Map.Map Text ToolCall
  , outputs :: Map.Map Text Value
  , finished :: Set.Set (Text, Text), committed :: Map.Map (Text, Text) ToolBatch
  , pendingDelegations :: Set.Set Text, finishedOrder :: Seq.Seq (Text, Text)
  } deriving (Eq, Show)

emptyToolState :: ToolState
emptyToolState = ToolState Map.empty Map.empty Map.empty Map.empty Set.empty Map.empty Set.empty Seq.empty

requiredText :: Int -> Text -> Value -> Result Text
requiredText limit key value = case textField key value of
  Just text | not (T.null text) && T.length text <= limit -> Right text
  _ -> Left (failure "invalid_live_tool_event")

stepTools :: ToolLimits -> Value -> ToolState -> Result (ToolState, [ToolBatch])
stepTools limits envelope state = case field "event" envelope of
  Just event -> handleToolEvent limits envelope event state
  Nothing -> Right (state, [])

handleToolEvent :: ToolLimits -> Value -> Value -> ToolState -> Result (ToolState, [ToolBatch])
handleToolEvent limits envelope event state
  | kind `notElem` kinds = Right (state, [])
  | otherwise = do
      delegation <- requiredText 200 "delegation_id" envelope
      let response = fromMaybe Null (field "response" event)
          current = textField "id" response <|> textField "response_id" event <|> Map.lookup delegation (activeResponses state)
      routeToolEvent limits delegation current kind response event state
  where
    kind = fromMaybe "" (textField "type" event)
    kinds = ["response.created", "response.output_item.done", "response.completed", "response.failed", "response.incomplete"]

routeToolEvent :: ToolLimits -> Text -> Maybe Text -> Text -> Value -> Value -> ToolState -> Result (ToolState, [ToolBatch])
routeToolEvent _ _ Nothing _ _ _ _ = Left (failure "missing_live_response_id")
routeToolEvent limits delegation (Just response) kind result event state
  | Set.member key (finished state) = Right (state, [])
  | kind == "response.created" = createBatch limits delegation response state
  | kind == "response.output_item.done" = addCall limits key event state
  | otherwise = finishBatch limits key kind result state
  where
    key = (delegation, response)

createBatch :: ToolLimits -> Text -> Text -> ToolState -> Result (ToolState, [ToolBatch])
createBatch limits delegation response state
  | T.null response || T.length response > 200 = Left (failure "invalid_live_response_id")
  | Map.notMember key (batches state) && Map.size (batches state) + Map.size (committed state) >= maxPendingResponses limits = Left (failure "live_pending_limit")
  | otherwise = Right (state { batches = Map.insertWith keep key batch (batches state),
                              activeResponses = Map.insert delegation response (activeResponses state),
                              pendingDelegations = Set.insert delegation (pendingDelegations state) }, [])
  where
    key = (delegation, response)
    batch = ToolBatch delegation response []
    keep _ old = old

addCall :: ToolLimits -> (Text, Text) -> Value -> ToolState -> Result (ToolState, [ToolBatch])
addCall limits key event state = case field "item" event of
  Just item | textField "type" item == Just "function_call" -> do
    ident <- requiredText 200 "call_id" item
    name <- requiredText 80 "name" item
    args <- requiredText (maxToolText limits) "arguments" item
    insertCall limits key (ToolCall ident name args) state
  _ -> Right (state, [])

insertCall :: ToolLimits -> (Text, Text) -> ToolCall -> ToolState -> Result (ToolState, [ToolBatch])
insertCall limits key call state
  | Just previous <- Map.lookup ident (knownCalls state), previous /= call = Left (failure "live_tool_id_reused")
  | Map.notMember ident (knownCalls state) && Map.size (knownCalls state) >= maxToolCalls limits = Left (failure "live_tool_limit")
  | otherwise = case Map.lookup key (batches state) of
      Nothing -> Left (failure "live_tool_without_response")
      Just batch -> appendCall limits key call batch state
  where
    ident = toolCallId call

appendCall :: ToolLimits -> (Text, Text) -> ToolCall -> ToolBatch -> ToolState -> Result (ToolState, [ToolBatch])
appendCall limits key call batch state
  | call `elem` batchCalls batch = Right (state, [])
  | length (batchCalls batch) >= maxResponseCalls limits = Left (failure "live_response_tool_limit")
  | otherwise = Right (state { batches = Map.insert key updated (batches state),
                              knownCalls = Map.insert (toolCallId call) call (knownCalls state) }, [])
  where
    updated = batch { batchCalls = batchCalls batch ++ [call] }

finishBatch :: ToolLimits -> (Text, Text) -> Text -> Value -> ToolState -> Result (ToolState, [ToolBatch])
finishBatch limits key kind result state = case Map.lookup key (batches state) of
  Nothing -> Right (state, [])
  Just batch -> Right (updated batch, [batch | complete && not (null (batchCalls batch))])
  where
    complete = kind == "response.completed" && textField "status" result `elem` [Nothing, Just "completed"]
    retired = retireResponses (maxToolCalls limits) [key] state {batches = Map.delete key (batches state),
                      activeResponses = Map.delete (fst key) (activeResponses state)}
    updated batch
      | complete && not (null (batchCalls batch)) = retired {committed = Map.insert key batch (committed state)}
      | otherwise = abandonCalls (batchCalls batch) cleared
      where cleared = retired {pendingDelegations = Set.delete (fst key) (pendingDelegations state)}

retireResponses :: Int -> [(Text, Text)] -> ToolState -> ToolState
retireResponses limit keys state = state {finished = Set.fromList (toList bounded), finishedOrder = bounded}
  where
    fresh = filter (`Set.notMember` finished state) keys
    appended = finishedOrder state Seq.>< Seq.fromList fresh
    excess = max 0 (Seq.length appended - limit)
    bounded = Seq.drop excess appended

rememberOutput :: Text -> Value -> ToolState -> ToolState
rememberOutput ident result state = state { outputs = Map.insert ident result (outputs state) }

cachedOutput :: Text -> ToolState -> Maybe Value
cachedOutput ident = Map.lookup ident . outputs

data InputFrame = InputFrame BS.ByteString Double
data PlaybackFrame = PlaybackFrame Int BS.ByteString

observeStarted :: Value -> Result Bool
observeStarted event = case textField "type" event of
  Just "session.started" -> Right True
  Just "error" -> Left (failure "live_setup_rejected")
  Just "session.closed" -> Left (failure "live_closed_before_start")
  _ -> Right False

timedInput :: LiveConfig -> Double -> BS.ByteString -> ([InputFrame], BS.ByteString)
timedInput cfg stamp bytes = (zipWith timed [0 :: Int ..] frames, rest)
  where
    (frames, rest) = splitFrames (liveFrameBytes cfg) bytes
    byteRate = fromIntegral (liveInputRate cfg * 2)
    firstAt = stamp - fromIntegral (BS.length bytes) / byteRate
    frameTime = fromIntegral (liveFrameMicros cfg) / 1000000
    timed index frame = InputFrame frame (firstAt + fromIntegral index * frameTime)

selectInput :: LiveConfig -> Bool -> Double -> Maybe InputFrame -> InputFrame
selectInput cfg suppressed stamp next = InputFrame frame recordedAt
  where
    silence = BS.replicate (liveFrameBytes cfg) 0
    fallbackAt = stamp - fromIntegral (liveFrameMicros cfg) / 1000000
    InputFrame received recordedAt = fromMaybe (InputFrame silence fallbackAt) next
    frame | suppressed = silence
          | otherwise = received

decodeEvent :: Int -> BS.ByteString -> Result Value
decodeEvent limit bytes = case decodeStrict' bytes of
  Just event@(Object _) | BS.length bytes <= limit && isJust (textField "type" event) -> Right event
  _ -> Left (failure "invalid_live_event")

validOutput :: Int -> Value -> Bool
validOutput limit value@(Object fields) = KM.lookup "status" fields `elem` map (Just . String) ["ok", "error", "unknown"]
  && T.length (TE.decodeUtf8 (BL.toStrict (encode value))) <= limit
validOutput _ _ = False

advertisedNames :: Value -> [Text]
advertisedNames session = case field "delegation" session >>= field "responses" >>= field "tools" of
  Just (Array tools) -> foldr (\tool names -> maybe names (: names) (textField "name" tool)) [] tools
  _ -> []

failureOutput :: LiveConfig -> Text -> Failure -> Value
failureOutput cfg name err = object (base ++ fields)
  where
    status | failureUncertain err = "unknown" :: Text
           | otherwise = "error"
    base = ["status" .= status, "error" .= failureCode err]
    applicable = Map.elems (Map.filterWithKey (\prefix _ -> prefix `T.isPrefixOf` name) (liveErrorFields cfg))
    fields = [(Key.fromText key, value) | values <- applicable, (key, value) <- Map.toList values]

pendingTools :: Text -> ToolState -> Bool
pendingTools ownId state = not (Map.null (batches state)) || any pending (Map.keys (knownCalls state))
  where pending ident = ident /= ownId && Map.notMember ident (outputs state)

trackInterruption :: Bool -> Value -> Bool
trackInterruption active event = active && maybe False (not . T.null . T.strip) delta
  where delta = eventDelta "session.input_transcript.delta" event

appendTranscript :: Value -> Maybe (Text -> Text)
appendTranscript event = fmap append (eventDelta "session.output_transcript.delta" event)
  where append delta = T.takeEnd 2000 . (<> delta)

numberedCommand :: Int -> Value -> Value
numberedCommand index (Object fields) = Object (KM.insert "event_id" (String ident) fields)
  where ident = "phone_tool_" <> T.pack (show index)
numberedCommand _ value = value

annotateTranscript :: Timeline -> Value -> Value
annotateTranscript clock event@(Object fields) = case (numberField "start_ms" event, numberField "end_ms" event) of
  (Just first, Just lastTime) -> case transcriptBounds first lastTime clock of
    Just (start, end) -> Object (KM.insert "archive_start" (Number (realToFrac start)) (KM.insert "archive_end" (Number (realToFrac end)) fields))
    Nothing -> event
  _ -> event
annotateTranscript _ event = event

recoverable :: Set.Set Text -> Value -> Bool
recoverable notices event = case field "error" event of
  Just err -> textField "type" err == Just "invalid_request_error" && textField "code" err == Just "invalid_request_error"
    && textField "message" err == Just "Responses handoff incomplete." && field "param" err `elem` [Nothing, Just Null]
    && maybe True (`Set.notMember` notices) (textField "client_event_id" err)
  Nothing -> False

abandonUncommitted :: Int -> ToolState -> ToolState
abandonUncommitted limit state = abandonCalls abandonedCalls cleared
  where
    abandonedCalls = concatMap batchCalls (Map.elems (batches state))
    protected = Set.fromList (map delegationId (Map.elems (committed state)))
    cleared = retireResponses limit (Map.keys (batches state)) state
      {batches = Map.empty, activeResponses = Map.empty, pendingDelegations = protected}

abandonCalls :: [ToolCall] -> ToolState -> ToolState
abandonCalls calls state = foldr abandon state calls
  where
    protected = Set.fromList (map toolCallId (concatMap batchCalls (Map.elems (committed state))))
    output = object ["status" .= ("error" :: Text), "error" .= ("delegation_incomplete_not_executed" :: Text)]
    abandon call current
      | Set.member ident protected || Map.member ident (outputs current) = current
      | otherwise = rememberOutput ident output current
      where ident = toolCallId call

startedEvent :: Value -> Word64 -> Word64 -> Value
startedEvent session started now = object
  ["type" .= ("phone.session_started" :: Text), "model" .= textField "model" session,
   "voice" .= voice, "backend_model" .= backend, "setup_ms" .= elapsed]
  where
    voice = field "audio" session >>= field "output" >>= textField "voice"
    backend = field "delegation" session >>= field "responses" >>= textField "model"
    elapsed = fromIntegral (now - started) / (1000000 :: Double)

closedEvent :: Word64 -> Word64 -> (Bool, Maybe Value) -> Value
closedEvent started now (closed, usage) = object
  ["type" .= ("phone.session_closed" :: Text), "finalized" .= closed,
   "final_usage" .= usage, "duration_ms" .= elapsed]
  where elapsed = fromIntegral (now - started) / (1000000 :: Double)

closedStatus :: Value -> (Bool, Maybe Value)
closedStatus event = (True, field "usage" event)
