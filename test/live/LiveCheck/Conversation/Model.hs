module LiveCheck.Conversation.Model where

import Baerbel.Core.JSON (field, jsonOptions, numberField, textField)
import Data.Aeson (ToJSON(..), Value(..), genericToJSON)
import Data.Char (isAlphaNum)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Data.Text as Text

-- Observations contain synthetic conversation content, never session configuration.
data Event = Event
  { eventAt :: Word64, eventKind :: Text, eventText :: Text
  , eventTool :: Maybe Text, eventStatus :: Maybe Text, eventMillis :: Maybe Double, eventFinalized :: Maybe Bool, eventQueueMs :: Maybe Double
  , eventItemType :: Maybe Text, eventItemStatus :: Maybe Text }

data Frame = Frame {frameAt :: Word64, frameBytes :: Int, frameSpeech :: Bool}

data Trace = Trace {traceEvents :: [Event], traceFrames :: [Frame], traceFinished :: Bool, traceInputBytes :: Int}

emptyTrace :: Trace
emptyTrace = Trace [] [] False 0

recordEvent :: Word64 -> Value -> Trace -> Trace
recordEvent now value trace = trace
  {traceEvents = event : traceEvents trace, traceInputBytes = traceInputBytes trace + inputBytes}
  where
    payload = case textField "type" value of
      Just "response.event" -> fromMaybe value (field "event" value)
      _ -> value
    kind = fromMaybe "unknown" (textField "type" payload)
    content = fromMaybe "" (textField "delta" payload)
    inputBytes | kind == "check.input.audio" = round (fromMaybe 0 (numberField "bytes" payload))
               | otherwise = 0
    elapsed = case numberField "elapsed_ms" payload of
      Just millis -> Just millis
      Nothing -> numberField "setup_ms" payload
    finalized = case field "finalized" payload of
      Just (Bool valueFinalized) -> Just valueFinalized
      _ -> Nothing
    event = Event now kind content (textField "tool" payload) (textField "status" payload) elapsed finalized (numberField "max_output_queue_ms" payload)
      (field "item" payload >>= textField "type") (field "item" payload >>= textField "status")

recordFrame :: Word64 -> Int -> Bool -> Trace -> Trace
recordFrame now bytes speech trace = trace {traceFrames = Frame now bytes speech : traceFrames trace}

markFinished :: Trace -> Trace
markFinished trace = trace {traceFinished = True}

textSince :: Text -> Word64 -> Trace -> Text
textSince kind since = Text.concat . map eventText . reverse . filter matching . traceEvents
  where matching event = eventKind event == kind && eventAt event >= since

answerSince :: Word64 -> Trace -> Text
answerSince = textSince "session.output_transcript.delta"

hasWords :: [Text] -> Text -> Bool
hasWords expected value = any (`elem` actual) expected
  where
    actual = Text.words (Text.map normalize (Text.toCaseFold value))
    normalize char | isAlphaNum char = char
                   | otherwise = ' '

hasPhrase :: Text -> Text -> Bool
hasPhrase phrase = Text.isInfixOf phrase . Text.toCaseFold

seen :: Text -> Trace -> Bool
seen kind = any ((== kind) . eventKind) . traceEvents

eventTimes :: Text -> Trace -> [Word64]
eventTimes kind = map eventAt . reverse . filter ((== kind) . eventKind) . traceEvents

calls :: Trace -> [Text]
calls = reverse . mapMaybe eventTool . filter ((== "check.tool.started") . eventKind) . traceEvents

audibleSince :: Word64 -> Trace -> Bool
audibleSince since = any (\frame -> frameAt frame >= since && frameSpeech frame) . traceFrames

quietAfter :: Word64 -> Word64 -> Trace -> Bool
quietAfter since now trace = latest >= since && now >= latest + 1200000000
  where
    speech = map frameAt (filter frameSpeech (traceFrames trace))
    text = eventTimes "session.output_transcript.delta" trace
    latest = foldr max 0 (speech ++ text)

answered :: Word64 -> [Text] -> Word64 -> Trace -> Bool
answered since wordsExpected now trace = hasWords wordsExpected (answerSince since trace)
  && audibleSince since trace && quietAfter since now trace

milliseconds :: Word64 -> Word64 -> Double
milliseconds start end = fromIntegral (end - start) / 1000000

data ToolTiming = ToolTiming
  { toolName :: Text, toolStatus :: Maybe Text, toolElapsedMs :: Maybe Double } deriving (Generic)
instance ToJSON ToolTiming where toJSON = genericToJSON (jsonOptions 4)

toolTiming :: Event -> Maybe ToolTiming
toolTiming event
  | eventKind event == "phone.tool" = (\name -> ToolTiming name (eventStatus event) (eventMillis event)) <$> eventTool event
  | otherwise = Nothing

data Summary = Summary
  { summarySetupMs :: Maybe Double, summaryFirstDeliveredAudioMs :: Maybe Double
  , summaryFirstDeliveredSpeechMs :: Maybe Double, summaryOutputAudioMs :: Double
  , summaryDeliveredFrames :: Int, summaryOutputFrameBytes :: [Int]
  , summaryTools :: [Text], summaryToolExecutions :: [ToolTiming]
  , summaryMaxOutputQueueMs :: Maybe Double, summaryBackendCompletions :: Int, summaryCompletedWebSearches :: Int, summaryProviderErrors :: Int
  , summaryInputTranscript :: Text, summaryOutputTranscript :: Text
  , summaryLocalCleanupObserved :: Bool, summaryProviderFinalized :: Bool, summaryFinishCallInvoked :: Bool
  } deriving (Generic)
instance ToJSON Summary where toJSON = genericToJSON (jsonOptions 7)

summarize :: Word64 -> Int -> Trace -> Summary
summarize start rate trace = Summary setup firstAudio firstSpeech duration (length frames) sizes
  (calls trace) executions queued completions (length (webSearchTimes trace)) errors input output (seen "phone.session_closed" trace) (any ((== Just True) . eventFinalized) (traceEvents trace)) (traceFinished trace)
  where
    frames = reverse (traceFrames trace)
    setup = find ((== "phone.session_started") . eventKind) (traceEvents trace) >>= eventMillis
    firstAudio = milliseconds start . frameAt <$> first frames
    firstSpeech = milliseconds start . frameAt <$> find frameSpeech frames
    duration = fromIntegral (sum (map frameBytes frames)) * 1000 / fromIntegral (rate * 2)
    sizes = nub (map frameBytes frames)
    executions = reverse (mapMaybe toolTiming (traceEvents trace))
    queued = find ((== "phone.call_summary") . eventKind) (traceEvents trace) >>= eventQueueMs
    completions = length (eventTimes "response.completed" trace)
    errors = length (filter (\event -> eventKind event `elem` ["error", "response.failed", "response.incomplete"]) (traceEvents trace))
    input = Text.takeEnd 1800 (textSince "session.input_transcript.delta" 0 trace)
    output = Text.takeEnd 1800 (answerSince 0 trace)

first :: [a] -> Maybe a
first [] = Nothing
first (value : _) = Just value

data TurnTiming = TurnTiming
  { turnFirstTranscriptMs :: Maybe Double, turnFirstDeliveredSpeechMs :: Maybe Double }
  deriving (Generic)
instance ToJSON TurnTiming where toJSON = genericToJSON (jsonOptions 4)

turnTiming :: Word64 -> Trace -> TurnTiming
turnTiming end trace = TurnTiming (milliseconds end <$> first transcript) (milliseconds end <$> first speech)
  where
    transcript = filter (>= end) (eventTimes "session.output_transcript.delta" trace)
    speech = map frameAt (reverse (filter (\frame -> frameAt frame >= end && frameSpeech frame) (traceFrames trace)))

webSearchTimes :: Trace -> [Word64]
webSearchTimes = map eventAt . reverse . filter completed . traceEvents
  where
    completed event = eventKind event == "response.output_item.done"
      && eventItemType event == Just "web_search_call" && eventItemStatus event == Just "completed"
