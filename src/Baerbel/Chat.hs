module Baerbel.Chat
  ( ChunkRules(..), ChatState(..), emptyChat, takeChunk, chatEvent, splitEvents, chatRequest ) where

import Baerbel.Error (Result, failure)
import Data.Aeson (Value(..), object, (.=), decodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

data ChunkRules = ChunkRules { chunkLimit :: Int, chunkAbbreviations :: [Text] } deriving (Eq, Show)
data ChatState = ChatState { chatBuffer :: Text, chatText :: Text, chatCompleted :: Bool } deriving (Eq, Show)

emptyChat :: ChatState
emptyChat = ChatState "" "" False

takeChunk :: ChunkRules -> Bool -> Text -> Maybe (Text, Text)
takeChunk rules final supplied = case boundaries rules final buffer of
  end : _ -> Just (divide end)
  [] | T.length buffer > chunkLimit rules -> Just (divide (wordLimit (chunkLimit rules) buffer))
  [] | final && not (T.null buffer) -> Just (buffer, "")
  [] -> Nothing
  where
    buffer = T.stripStart supplied
    divide index = let (piece, rest) = T.splitAt index buffer in (T.strip piece, T.stripStart rest)

wordLimit :: Int -> Text -> Int
wordLimit limit text = case T.findIndex isSpace (T.reverse (T.take (limit + 1) text)) of
  Just index | index < limit -> limit - index
  _ -> limit

boundaries :: ChunkRules -> Bool -> Text -> [Int]
boundaries rules final buffer = filter accepted candidates
  where
    candidates = punctuationEnds 0 (T.unpack buffer)
    accepted end = end <= chunkLimit rules && (final || end < T.length buffer)
      && not (abbreviation rules (T.take end buffer))

punctuationEnds :: Int -> String -> [Int]
punctuationEnds _ [] = []
punctuationEnds offset (char : rest)
  | char == '\n' = offset + 1 : punctuationEnds (offset + 1) rest
  | char `elem` (".!?" :: String) = following end remaining
  | otherwise = punctuationEnds (offset + 1) rest
  where
    (suffix, remaining) = span (`elem` (".!?\"')]”’" :: String)) rest
    end = offset + 1 + length suffix
    following index [] = [index]
    following index chars@(next : _) = [index | isSpace next] ++ punctuationEnds index chars

abbreviation :: ChunkRules -> Text -> Bool
abbreviation rules prefix = case T.unsnoc prefix of
  Just (before, '.') -> let word = T.takeWhileEnd (\char -> isAlpha char || char == '.') before
                       in T.toLower word `elem` chunkAbbreviations rules || T.length word == 1 || dotted word
  _ -> False
  where
    dotted word = T.any (== '.') word && all ((== 1) . T.length) (T.splitOn "." word)

chatEvent :: ChunkRules -> Value -> ChatState -> Result (ChatState, [Text])
chatEvent rules (Object fields) state = case KM.lookup "type" fields of
  Just (String kind) | kind `elem` ["response.output_text.delta", "response.refusal.delta"] ->
    case KM.lookup "delta" fields of
      Just (String delta) -> Right (flush rules False state {chatText = chatText state <> delta, chatBuffer = chatBuffer state <> delta})
      _ -> Left (failure "invalid_chat_event")
  Just (String kind) | kind `elem` ["response.output_text.done", "response.refusal.done"] -> Right (flush rules True state)
  Just (String "response.completed") -> completeChat rules fields state
  Just (String kind) | kind `elem` ["response.failed", "response.incomplete", "error"] -> Left (failure "chat_response_failed")
  _ -> Right (state, [])
chatEvent _ _ _ = Left (failure "invalid_chat_event")

completeChat :: ChunkRules -> KM.KeyMap Value -> ChatState -> Result (ChatState, [Text])
completeChat rules fields state = case KM.lookup "response" fields of
  Just (Object response) | KM.lookup "status" response == Just (String "completed") && not (T.null (T.strip (chatText state))) ->
    Right (flush rules True state {chatCompleted = True})
  _ -> Left (failure "chat_response_incomplete")

flush :: ChunkRules -> Bool -> ChatState -> (ChatState, [Text])
flush rules final state = case takeChunk rules final (chatBuffer state) of
  Nothing -> (state, [])
  Just (piece, rest) -> let (next, pieces) = flush rules final state {chatBuffer = rest}
                        in (next, piece : pieces)

splitEvents :: BS.ByteString -> Result ([Value], BS.ByteString)
splitEvents bytes = do
  let linesOf = BS.split 10 bytes
      complete = take (max 0 (length linesOf - 1)) linesOf
      rest = BS.drop (BS.length bytes - remainderSize linesOf) bytes
  events <- traverse decodeLine (filter ("data:" `BS.isPrefixOf`) complete)
  pure (concat events, rest)
  where
    remainderSize [] = 0
    remainderSize values = foldl (\_ value -> BS.length value) 0 values
    decodeLine line
      | value == "[DONE]" || value == "[DONE]\r" = Right []
      | otherwise = maybe (Left (failure "invalid_chat_stream")) (Right . (: [])) (decodeStrict' value)
      where value = BS.dropWhile (`elem` [13, 32]) (BS.drop 5 line)

chatRequest :: Text -> Text -> Text -> Int -> [Value] -> Value
chatRequest model instructions effort tokens history = object (base ++ reasoning)
  where
    base = ["model" .= model, "instructions" .= instructions, "input" .= history,
            "max_output_tokens" .= tokens, "store" .= False, "stream" .= True]
    reasoning = ["reasoning" .= object ["effort" .= effort] |
      any (`T.isPrefixOf` model) ["gpt-5", "gpt-6"] && not (T.null effort)]
