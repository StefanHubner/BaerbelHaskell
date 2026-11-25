module Baerbel.MailContent (htmlText, htmlTextWith, decodeWords, decodeCharset) where

import Baerbel.Error (Result, failure, ioResult)
import Control.Exception (evaluate, try)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Char (isAscii, isAlphaNum)
import Data.Either (fromRight)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.ICU.Convert as ICU
import Data.Text.ICU.Error (ICUError)
import GHC.ByteOrder (ByteOrder(..), targetByteOrder)
import qualified GHC.Foreign as Foreign
import System.IO (mkTextEncoding)
import Text.HTML.TagSoup (Tag(..), parseTags)

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

-- Fresh converters stay inside IO: ICU converters carry mutable decoding state.
decodeCharset :: Text -> ByteString -> IO Text
decodeCharset charset bytes = do
  let (name, input) = encodingInput charset bytes
      decode | unicodeName name = unicodeBody name input
             | otherwise = systemDecode True name input
  result <- decode
  either (const (fallbackBody bytes)) pure result

fallbackBody :: ByteString -> IO Text
fallbackBody bytes = do
  result <- unicodeBody "utf-8" bytes
  let fallback = Text.decodeUtf8With lenientDecode bytes
  pure (fromRight fallback result)

unicodeBody :: Text -> ByteString -> IO (Result Text)
unicodeBody name bytes = do
  result <- try (convertUnicode name bytes)
  pure (either failed Right result)
  where
    failed :: ICUError -> Result Text
    failed _ = Left (failure "invalid_message_charset")

convertUnicode :: Text -> ByteString -> IO Text
convertUnicode name bytes = do
  converter <- ICU.open (Text.unpack name) Nothing
  let decoded = ICU.toUnicode converter bytes
  evaluate decoded

systemDecode :: Bool -> Text -> ByteString -> IO (Result Text)
systemDecode replace name bytes
  | Text.null name || Text.any invalidName name = pure (Left (failure "invalid_message_header"))
  | otherwise = ioResult "invalid_message_header" convert
  where
    invalidName char = not (isAscii char && (isAlphaNum char || char `elem` ("-_." :: String)))
    selected | replace = Text.unpack name <> "//TRANSLIT"
             | otherwise = Text.unpack name
    convert = do
      codec <- mkTextEncoding selected
      decoded <- Bytes.useAsCStringLen bytes (Foreign.peekCStringLen codec)
      evaluate (Text.pack decoded)

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

decodeWords :: Text -> IO (Result Text)
decodeWords input = runExceptT $ do
  pieces <- except (parseWords input)
  decoded <- traverse (ExceptT . renderPiece) pieces
  pure (Text.concat decoded)

renderPiece :: HeaderPiece -> IO (Result Text)
renderPiece (Literal value) = pure (Right value)
renderPiece (Encoded charset bytes)
  | Text.toLower charset == "unknown-8bit" = systemDecode True "ascii" bytes
  | otherwise = let (name, input) = encodingInput charset bytes in systemDecode False name input

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
