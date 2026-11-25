module Baerbel.Core.MailContent where

import Baerbel.Core.Error ( Result, failure )
import Data.ByteArray.Encoding ( Base(..), convertFromBase )
import Data.ByteString ( ByteString )
import Data.Char ( isAscii, isAlphaNum )
import Data.Text ( Text )
import GHC.ByteOrder ( ByteOrder(..), targetByteOrder )
import Text.HTML.TagSoup ( Tag(..), parseTags )
import qualified Data.ByteString.Base64 as Base64 ( decode )
import qualified Data.ByteString as Bytes
    ( break, drop, isPrefixOf, length, null, pack, replicate, take )
import qualified Data.Text as Text
    ( any,
      breakOn,
      concat,
      drop,
      filter,
      isPrefixOf,
      null,
      replace,
      splitOn,
      stripStart,
      toCaseFold,
      toLower,
      unpack )
import qualified Data.Text.Encoding as Text
    ( decodeUtf8', encodeUtf8 )

htmlText :: Text -> Text
htmlText = htmlTextWith ["head", "script", "style"] ["p", "div", "br", "li", "tr", "h1", "h2", "h3"] ["p", "div", "li", "tr"]

htmlTextWith :: [Text] -> [Text] -> [Text] -> Text -> Text
htmlTextWith hidden opening closing input = Text.concat (visible hidden opening closing 0 (parseTags input))

visible :: [Text] -> [Text] -> [Text] -> Int -> [Tag Text] -> [Text]
visible _ _ _ _ [] = []
visible hidden opening closing depth (tag : rest) = output <> visible hidden opening closing next rest
  where
    (next, output) = case tag of
      TagOpen name _ | Text.toLower name `elem` hidden -> (depth + 1, [])
      TagClose name | Text.toLower name `elem` hidden -> (max 0 (depth - 1), [])
      TagOpen name _ | depth == 0 && Text.toLower name `elem` opening -> (depth, ["\n"])
      TagClose name | depth == 0 && Text.toLower name `elem` closing -> (depth, ["\n"])
      TagText value | depth == 0 -> (depth, [value])
      _ -> (depth, [])

data Decoder = Decoded Text | Unicode Text ByteString | System String ByteString

bodyDecoder :: Text -> ByteString -> Result Decoder
bodyDecoder charset bytes
  | name == "utf-8", Right value <- Text.decodeUtf8' input = Right (Decoded value)
  | unicodeName name = Right (Unicode name input)
  | otherwise = systemDecoder True name input
  where (name, input) = encodingInput charset bytes

systemDecoder :: Bool -> Text -> ByteString -> Result Decoder
systemDecoder replace name bytes
  | Text.null name || Text.any invalidName name = invalid
  | not replace && name == "utf-8" = either (const invalid) (Right . Decoded) (Text.decodeUtf8' bytes)
  | otherwise = Right (System selected bytes)
  where
    invalid = Left (failure "invalid_message_header")
    invalidName char = not (isAscii char && (isAlphaNum char || char `elem` ("-_." :: String)))
    selected | replace = Text.unpack name <> "//TRANSLIT"
             | otherwise = Text.unpack name

unicodeName :: Text -> Bool
unicodeName name = any (`Text.isPrefixOf` name) ["utf-8", "utf-16", "utf-32"]

encodingInput :: Text -> ByteString -> (Text, ByteString)
encodingInput charset bytes = case compact of
  "utf8sig" -> ("utf-8", dropPrefix [239, 187, 191] bytes)
  "utf8" -> ("utf-8", bytes)
  "cp65001" -> ("utf-8", bytes)
  "utf16" -> (nativeUnicode "utf-16" [[255, 254], [254, 255]] bytes, bytes)
  "utf32" -> (nativeUnicode "utf-32" [[255, 254, 0, 0], [0, 0, 254, 255]] bytes, bytes)
  "utf16le" -> ("utf-16le", bytes)
  "utf16be" -> ("utf-16be", bytes)
  "utf32le" -> ("utf-32le", bytes)
  "utf32be" -> ("utf-32be", bytes)
  "latin1" -> ("iso-8859-1", bytes)
  _ -> (name, bytes)
  where
    name = Text.toLower (Text.replace "_" "-" charset)
    compact = Text.filter (/= '-') name

nativeUnicode :: Text -> [[Word]] -> ByteString -> Text
nativeUnicode name marks bytes
  | any starts marks = name
  | otherwise = name <> case targetByteOrder of LittleEndian -> "le"; BigEndian -> "be"
  where starts mark = let prefix = Bytes.pack (map fromIntegral mark) in prefix `Bytes.isPrefixOf` bytes

dropPrefix :: [Word] -> ByteString -> ByteString
dropPrefix mark bytes
  | prefix `Bytes.isPrefixOf` bytes = Bytes.drop (Bytes.length prefix) bytes
  | otherwise = bytes
  where prefix = Bytes.pack (map fromIntegral mark)

data HeaderPiece = Literal Text | Encoded Text ByteString

renderPiece :: HeaderPiece -> Result Decoder
renderPiece (Literal value) = Right (Decoded value)
renderPiece (Encoded charset bytes)
  | Text.toLower charset == "unknown-8bit" = systemDecoder True "ascii" bytes
  | otherwise = let (name, input) = encodingInput charset bytes in systemDecoder False name input

parseWords :: Text -> Result [HeaderPiece]
parseWords input = case Text.breakOn "=?" input of
  (_, "") -> Right [Literal input]
  (prefix, encoded) -> parseWord prefix encoded

parseWord :: Text -> Text -> Result [HeaderPiece]
parseWord prefix encoded = case Text.splitOn "?" (Text.drop 2 word) of
  [charset, encoding, content] -> do
    bytes <- decodeTransfer (Text.toCaseFold encoding) content
    decodedSuffix <- parseWords after
    Right (Literal prefix : Encoded charset bytes : decodedSuffix)
  _ -> Right [Literal (prefix <> encoded)]
  where
    (word, suffix) = Text.breakOn "?=" encoded
    remaining = Text.drop 2 suffix
    stripped = Text.stripStart remaining
    after | "=?" `Text.isPrefixOf` stripped = stripped
          | otherwise = remaining

decodeTransfer :: Text -> Text -> Result ByteString
decodeTransfer "b" encoded = either (const invalid) Right (Base64.decode padded)
  where
    bytes = Text.encodeUtf8 encoded
    padding = (4 - Bytes.length bytes `mod` 4) `mod` 4
    padded = bytes <> Bytes.replicate padding 61
    invalid = Left (failure "invalid_message_header")
decodeTransfer "q" encoded = quotedPrintable (Text.encodeUtf8 (Text.replace "_" " " encoded))
decodeTransfer _ encoded = Right (Text.encodeUtf8 encoded)

quotedPrintable :: ByteString -> Result ByteString
quotedPrintable bytes = case Bytes.break (== 61) bytes of
  (_, suffix) | Bytes.null suffix -> Right bytes
  (prefix, suffix) -> do
    let pair = Bytes.take 2 (Bytes.drop 1 suffix)
        following = Bytes.drop 3 suffix
    decoded <- either (const (Left (failure "invalid_message_header"))) Right (convertFromBase Base16 pair)
    rest <- quotedPrintable following
    Right (prefix <> decoded <> rest)
