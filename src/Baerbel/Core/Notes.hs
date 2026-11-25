{-# LANGUAGE TupleSections #-}
module Baerbel.Core.Notes where

import Baerbel.Core.Error (Failure(..))
import Baerbel.Core.JSON (jsonOptions)
import Data.Aeson hiding (Result)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isAlphaNum)
import Data.List (tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Fragment = Fragment
  { fragmentSequence :: Int, fragmentText :: Text
  , fragmentStart :: Maybe Double, fragmentEnd :: Maybe Double } deriving (Eq, Show)
data Transcript = Transcript
  { transcriptFragments :: [Fragment], transcriptLastDelta :: Double
  , transcriptVoiceStarted :: Double, transcriptNow :: Double, transcriptArchiveBound :: Bool
  , transcriptSequence :: Int, transcriptDroppedThrough :: Int } deriving (Eq, Show)
data Rejection = Rejection
  { rejectionInstruction :: Maybe Text, rejectionObserved :: Bool } deriving (Generic)
instance FromJSON Rejection where parseJSON = genericParseJSON (jsonOptions 9)
data NoteRules = NoteRules
  { noteMaximumSeconds :: Double, noteMaximumBytes :: Int
  , noteSettleSeconds :: Double, noteWaitSeconds :: Int, noteDeliverySeconds :: Int
  , noteSelectionInstruction :: Text, noteMaximumQuoteChars :: Int
  , noteObservedTailChars :: Int, notePollMicros :: Int, noteRejections :: Map Text Rejection } deriving (Generic)
instance FromJSON NoteRules where parseJSON = genericParseJSON (jsonOptions 4)
data Phase = Idle | Recording | Limit | Sending | Sent | Failed deriving (Eq, Show)
data Selection = Selection
  { selectionId :: Text, selectionSequence :: Int, selectionAfter :: Maybe Double } deriving (Eq, Show)
data NoteState = NoteState
  { notePhase :: Phase, noteSelection :: Maybe Selection
  , noteReceipt :: Maybe Value, noteClosed :: Bool }
data Request = Start Text | Send Text | Cancel
data Reply = Rejected Text | Completed Value
data Draft = Draft { draftSelection :: Selection, draftParts :: [Fragment], draftSpeech :: Text, draftVoice :: Double }
data Prepared = Prepared { preparedId :: Text, preparedSpeech :: Text, preparedBegin :: Double, preparedEnd :: Double }
data Decision = Immediate Reply | Settle Draft
normalize :: Text -> [Text]
normalize = Text.words . Text.map replace . Text.filter (`notElem` ("'’" :: String)) . Text.toCaseFold
  where
    replace char | wordCharacter char = char
                 | otherwise = ' '

completionMatches :: Text -> Text -> Bool
completionMatches phrase speech = not (null quote) && drop offset spokenWords == quote
  where
    quote = normalize phrase
    spokenWords = normalize speech
    offset = length spokenWords - length quote

quoteSequence :: Text -> [Fragment] -> Maybe Int
quoteSequence phrase fragments = case matches of
  [position] -> snd <$> firstToken (drop position tokens)
  _ -> Nothing
  where
    characters = concatMap (\part -> map (, fragmentSequence part) (Text.unpack (fragmentText part))) fragments
    tokens = speechTokens characters
    quote = normalize phrase
    matches = [index |
                 not (null quote),
                 (index, suffix) <- zip [0 .. ] (tails (map fst tokens)),
                 take (length quote) suffix == quote]
    firstToken [] = Nothing
    firstToken (value : _) = Just value

speechTokens :: [(Char, Int)] -> [(Text, Int)]
speechTokens [] = []
speechTokens input@((char, position) : rest)
  | not (wordCharacter char) = speechTokens rest
  | otherwise = (token, position) : speechTokens remaining
  where
    (characters, remaining) = spanToken input
    token = Text.concat (normalize (Text.pack (map fst characters)))

wordCharacter :: Char -> Bool
wordCharacter char = isAlphaNum char || char == '_'

spanToken :: [(Char, Int)] -> ([(Char, Int)], [(Char, Int)])
spanToken [] = ([], [])
spanToken input@(first@(char, _) : rest)
  | wordCharacter char || apostrophe char rest = let (word, remaining) = spanToken rest in (first : word, remaining)
  | otherwise = ([], input)
  where
    apostrophe quote ((next, _) : _) = quote `elem` ("'’" :: String) && wordCharacter next
    apostrophe _ [] = False

selected :: Selection -> Transcript -> [Fragment]
selected selection transcript = filter included (transcriptFragments transcript)
  where
    included part = fragmentSequence part >= selectionSequence selection && after part
    after part = case (selectionAfter selection, fragmentEnd part) of
      (Just earliest, Just latest) -> latest > earliest
      _ -> True

synchronize :: Transcript -> NoteState -> NoteState
synchronize transcript state
  | notePhase state == Recording && maybe False lost (noteSelection state) = state {notePhase = Limit}
  | otherwise = state
  where lost selection = transcriptDroppedThrough transcript >= selectionSequence selection

validateRequest :: NoteRules -> NoteState -> Text -> Value -> Either Text Request
validateRequest _ state _ _ | noteClosed state = Left "invalid_note_request"
validateRequest rules _ name (Object arguments) = case name of
  "owner_note_start" -> Start <$> quoteArgument rules True "start_quote" arguments
  "owner_note_send" -> Send <$> quoteArgument rules False "completion_phrase" arguments
  "owner_note_cancel" | KeyMap.null arguments -> Right Cancel
                      | otherwise -> Left "invalid_note_request"
  _ -> Left "unknown_note_tool"
validateRequest _ _ _ _ = Left "invalid_note_request"

quoteArgument :: NoteRules -> Bool -> Key -> Object -> Either Text Text
quoteArgument rules empty key arguments = case KeyMap.lookup key arguments of
  Just (String phrase) | KeyMap.size arguments == 1 && validQuote rules empty phrase -> Right phrase
  _ -> Left code
  where code | empty = "start_quote_required"
             | otherwise = "completion_phrase_required"

validQuote :: NoteRules -> Bool -> Text -> Bool
validQuote rules empty phrase = Text.length phrase <= noteMaximumQuoteChars rules
  && not (Text.any invalid phrase) && (not (null (normalize phrase)) || empty && Text.null (Text.strip phrase))
  where invalid char = char < ' ' && char `notElem` ("\t\r\n" :: String)

select :: Text -> Text -> Transcript -> NoteState -> Either Text NoteState
select ident phrase transcript state
  | noteClosed state = Left "invalid_note_request"
  | notePhase state `notElem` [Idle, Sent, Failed] = Left "note_already_active"
  | not (transcriptArchiveBound transcript) = Left "call_recording_unavailable"
  | otherwise = makeState <$> begin
  where
    history = transcriptFragments transcript
    latest = foldl (\_ value -> fragmentEnd value) Nothing history
    prospective = Text.null (Text.strip phrase)
    begin | prospective = Right (transcriptSequence transcript + 1)
          | otherwise = maybe (Left "start_quote_not_unique") Right (quoteSequence phrase history)
    after | prospective = latest
          | otherwise = Nothing
    makeState position = NoteState Recording (Just (Selection ident position after)) Nothing False

sendDecision :: Text -> Transcript -> NoteState -> Decision
sendDecision phrase transcript state
  | noteClosed state = Immediate (Rejected "invalid_note_request")
  | Just receipt <- noteReceipt state = Immediate (Completed receipt)
  | notePhase state == Sending = Immediate (Completed alreadySending)
  | notePhase state == Limit = Immediate (Rejected "note_selection_exceeds_history_limit")
  | notePhase state /= Recording = Immediate (Rejected "note_not_selected")
  | otherwise = maybe (Immediate (Rejected "note_not_selected")) prepare (noteSelection state)
  where
    prepare selection
      | Text.null (Text.strip speech) = Immediate (Rejected "note_has_no_transcript")
      | not (completionMatches phrase speech) = Immediate (Rejected "completion_phrase_not_observed")
      | otherwise = Settle (Draft selection parts speech (transcriptVoiceStarted transcript))
      where
        parts = selected selection transcript
        speech = Text.concat (map fragmentText parts)

confirmDraft :: NoteRules -> Draft -> Maybe Transcript -> NoteState -> Either Text Prepared
confirmDraft _ _ Nothing _ = Left "caller_still_speaking_or_note_changed"
confirmDraft rules draft (Just transcript) state
  | noteClosed state || notePhase state /= Recording || noteSelection state /= Just selection = changed
  | transcriptVoiceStarted transcript /= draftVoice draft || selected selection transcript /= parts = changed
  | otherwise = timings rules (selectionId selection) (Text.strip (draftSpeech draft)) parts
  where
    selection = draftSelection draft
    parts = draftParts draft
    changed = Left "caller_still_speaking_or_note_changed"

timings :: NoteRules -> Text -> Text -> [Fragment] -> Either Text Prepared
timings rules ident speech parts = do
  values <- traverse timing parts
  case values of
    [] -> Left "call_recording_timing_unavailable"
    (begin, firstEnd) : rest -> valid begin (foldl (\_ value -> snd value) firstEnd rest)
  where
    timing part = case (fragmentStart part, fragmentEnd part) of
      (Just begin, Just end) -> Right (begin, end)
      _ -> Left "call_recording_timing_unavailable"
    valid begin end
      | end <= begin || end - begin > noteMaximumSeconds rules = Left "note_excerpt_duration_invalid"
      | otherwise = Right (Prepared ident speech begin end)

settled :: NoteRules -> Transcript -> Bool
settled rules transcript = transcriptNow transcript - transcriptLastDelta transcript >= noteSettleSeconds rules
  && transcriptVoiceStarted transcript <= transcriptLastDelta transcript

cancelSelection :: NoteState -> Either Reply NoteState
cancelSelection state
  | noteClosed state = Left (Rejected "invalid_note_request")
  | notePhase state == Sending = Left (Completed alreadySending)
  | notePhase state == Sent = Left (Rejected "note_already_sent")
  | otherwise = Right (NoteState Idle Nothing Nothing False)

sentReceipt :: Value -> Bool
sentReceipt (Object fields) = KeyMap.lookup "result" fields == Just (String "sent")
sentReceipt _ = False

finishReply :: NoteRules -> Text -> Text -> Transcript -> NoteState -> (Text, Text, Value)
finishReply rules name code transcript state = (action, phase, rejection rules code transcript state)
  where
    action | name `elem` ["owner_note_start", "owner_note_send", "owner_note_cancel"] = Text.drop 11 name
           | otherwise = "unknown"
    phase = Text.toLower (Text.pack (show (notePhase state)))

rejection :: NoteRules -> Text -> Transcript -> NoteState -> Value
rejection rules code transcript state = object (base <> instruction <> observed)
  where
    policy = Map.lookup code (noteRejections rules)
    instruction = maybe [] (\value -> ["instruction" .= value]) (policy >>= rejectionInstruction)
    observed = ["observed_tail" .= observedTail rules transcript state | maybe False rejectionObserved policy]
    base = ["status" .= ("error" :: Text), "error" .= code]

observedTail :: NoteRules -> Transcript -> NoteState -> Text
observedTail rules transcript state = Text.takeEnd (noteObservedTailChars rules) speech
  where
    parts = maybe [] (`selected` transcript) (noteSelection state)
    history = case parts of
      [] -> transcriptFragments transcript
      _ -> parts
    speech = Text.concat (map fragmentText history)

selectionResult :: NoteRules -> Value
selectionResult rules = object ["status" .= ("ok" :: Text), "result" .= ("selected" :: Text),
  "instruction" .= noteSelectionInstruction rules, "maximum_seconds" .= noteMaximumSeconds rules]

result :: Text -> Value
result code = object ["status" .= ("ok" :: Text), "result" .= code]

errorValue :: Text -> Text -> Value
errorValue status code = object ["status" .= status, "error" .= code]

failureValue :: Failure -> Value
failureValue issue = errorValue status (failureCode issue)
  where status | failureUncertain issue = "unknown"
               | otherwise = "error"

alreadySending :: Value
alreadySending = errorValue "unknown" "note_already_sending_do_not_retry"

unknownOutcome :: Value
unknownOutcome = errorValue "unknown" "note_send_outcome_unknown_do_not_retry"

excerptUnavailable :: Value
excerptUnavailable = errorValue "error" "call_recording_excerpt_unavailable"
