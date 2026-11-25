module Baerbel.Core.Events where

import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.Incoming (callbackNumber)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Csv as CSV
import Data.Char (isAscii, isAlphaNum, isDigit, isUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data EventRules = EventRules
  { eventMaximumLine :: Int, eventMaximumBatch :: Int, eventRotations :: Int
  , eventAnchorBytes :: Int, eventPollMicros :: Int, eventReasons :: Map Text Text
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
  selected | Map.member reason (eventReasons rules) = reason
           | otherwise = "UNKNOWN"

validIdentifier :: EventRules -> Text -> Bool
validIdentifier rules ident = case Text.splitOn "." ident of
  [first] -> part (eventIdentifierChars rules) first
  [first, second] -> part (eventIdentifierChars rules) first && part (eventSuffixChars rules) second
  _ -> False
  where part bound value = not (Text.null value) && Text.length value <= bound && Text.all (\char -> isAscii char && isAlphaNum char || char == '_' || char == '-') value

resumePosition :: Integer -> Integer -> Text -> Text -> Integer
resumePosition size position saved actual
  | size >= position && actual == saved = position
  | otherwise = 0

digest :: ByteString -> Text
digest bytes = Text.pack (show (hash bytes :: Digest SHA256))

recordRow :: EventRules -> Text -> Integer -> Int -> ByteString -> ByteString -> Row
recordRow rules key position count raw anchor = Row (parseEvent rules raw) cursor
  where cursor = Cursor key position count (digest anchor)
