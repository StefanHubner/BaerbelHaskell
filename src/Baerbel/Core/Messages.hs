module Baerbel.Core.Messages where

import Baerbel.Core.Incoming (Decision(..))
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.SMS (Delivery(..), Kind(..), Receipt(..))
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data MessageRules = MessageRules
  { messageReasons :: Map Text Text, messageDeclined :: Map Text Text
  , messageForwarded :: Map Text Text, messageUnknownNames :: [Text]
  , messageTemplates :: Map Text Text, messageExcerptChars :: Int
  , messageNameChars :: Int, messageSummaryChars :: Int
  , messageContextLimit :: Int, messageTaskLimit :: Int
  , messageWaitMicros :: Int, messageDeliveryMicros :: Int } deriving (Generic)
instance FromJSON MessageRules where parseJSON = genericParseJSON (jsonOptions 7)
data ScreeningContext = ScreeningContext
  { contextName :: Text, contextSummary :: Text, contextDecision :: Text
  , contextReason :: Text, contextKnown :: Bool, contextExcerpt :: Text
  , contextTruncated :: Bool, contextActive :: Int } deriving (Eq, Show)
emptyContext :: ScreeningContext
emptyContext = ScreeningContext "" "" "" "" False "" False 0

unknownReceipt :: Receipt
unknownReceipt = Receipt Uncertain Nothing Nothing

render :: MessageRules -> Text -> [(Text, Text)] -> Text
render rules key = foldl replace template
  where
  template = Map.findWithDefault "" key (messageTemplates rules)
  replace text (name, value) = Text.replace ("{" <> name <> "}") value text

cleanName :: MessageRules -> Text -> Text
cleanName rules value
  | Text.toCaseFold name `elem` messageUnknownNames rules = ""
  | otherwise = name
  where name = Text.strip value

sentence :: Text -> Text
sentence value
  | any (`Text.isSuffixOf` text) [".", "!", "?", "…"] = text
  | otherwise = text <> "."
  where text = Text.unwords (Text.words value)

outcomeText :: MessageRules -> Text -> Maybe Text -> ScreeningContext -> Text
outcomeText rules reason callback context = Text.intercalate " " parts
  where
  name = cleanName rules (contextName context)
  number = fromMaybe "" callback
  named | Text.null number = name
        | otherwise = name <> " (" <> number <> ")"
  identityKey | not (Text.null name) = "named"
              | Text.null number = "anonymous"
              | otherwise = "number"
  identity = render rules identityKey [("name", named), ("number", number)]
  base = Map.findWithDefault "" reason (messageReasons rules)
  description = describeOutcome rules reason context base
  excerpt = Text.unwords (Text.words (contextExcerpt context))
  quote | contextTruncated context = "…" <> excerpt
        | otherwise = excerpt
  detail | Text.null (contextSummary context) = render rules "excerpt" [("text", sentence quote)]
         | otherwise = sentence (contextSummary context)
  hasDetail = not (Text.null excerpt && Text.null (contextSummary context))
  parts = [identity] ++ [detail | hasDetail] ++ [description]

describeOutcome :: MessageRules -> Text -> ScreeningContext -> Text -> Text
describeOutcome rules reason context fallback
  | reason == "SCREENING" && contextKnown context = render rules "known_hangup" []
  | reason `elem` ["DECLINED", "SCREENING_DECLINED"] = Map.findWithDefault fallback (contextReason context) (messageDeclined rules)
  | contextDecision context == "forward" = Map.findWithDefault fallback reason (messageForwarded rules)
  | otherwise = fallback

chooseContexts :: Int -> Map Text a -> [(Text, Bool)] -> Maybe (Map Text a)
chooseContexts limit contexts available
  | Map.size contexts < limit = Just contexts
  | otherwise = case [key | (key, True) <- available] of
      key : _ -> Just (Map.delete key contexts)
      [] -> Nothing

finish :: ScreeningContext -> ScreeningContext
finish current = current {contextActive = max 0 (contextActive current - 1)}

appendExcerpt :: MessageRules -> Text -> ScreeningContext -> ScreeningContext
appendExcerpt rules delta current = current
  {contextExcerpt = Text.takeEnd bound text, contextTruncated = contextTruncated current || Text.length text > bound}
  where
    clean = Text.filter allowed delta
    allowed char = (ord char >= 32 || char == '\n' || char == '\t') && not (ord char >= 127 && ord char <= 159) && not (ord char >= 0xd800 && ord char <= 0xdfff)
    text = contextExcerpt current <> clean
    bound = messageExcerptChars rules

decided :: MessageRules -> Decision -> ScreeningContext -> ScreeningContext
decided _ (Forward reason name summary) = screenFacts "forward" reason name summary
decided _ (Decline reason name summary) = screenFacts "decline" reason name summary
decided rules (LeaveMessage name body) = \current -> current
  { contextDecision = "message", contextReason = "message", contextKnown = False
  , contextName = Text.take (messageNameChars rules) name
  , contextSummary = Text.take (messageSummaryChars rules) (Text.unwords (Text.words body)) }

screenFacts :: Text -> Text -> Text -> Text -> ScreeningContext -> ScreeningContext
screenFacts decision reason name summary current = current
  { contextDecision = decision, contextReason = reason
  , contextName = retain name (contextName current)
  , contextSummary = retain summary (contextSummary current)
  , contextKnown = Text.null name && contextKnown current }
  where
    retain provided previous
      | Text.null provided = previous
      | otherwise = provided

known :: Text -> ScreeningContext -> ScreeningContext
known name current = current {contextKnown = True, contextName = name, contextDecision = "forward"}

unsubmitted :: Kind -> Receipt
unsubmitted Outcome = Receipt Failed Nothing (Just True)
unsubmitted _ = Receipt Failed Nothing Nothing

normalizeReceipt :: Receipt -> Receipt
normalizeReceipt receipt = receipt {receiptStatus = status, receiptRetryable = retryable}
  where
    status = case receiptStatus receipt of
      Skipped -> Uncertain
      value -> value
    retryable = case receiptRetryable receipt of
      Just True -> Just True
      _ -> Nothing
