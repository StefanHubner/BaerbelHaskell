module Baerbel.JSON
  ( jsonOptions, camelOptions, argumentOptions, resultValue, decodeValue, decodeBytes, encodeBytes, encodeReference, decodeUnique, decodeUniqueIntegers ) where

import Baerbel.Error (Result, failure)
import Data.Aeson (FromJSON, ToJSON(..), Value, Options (..), defaultOptions, genericToJSON,
                   eitherDecodeStrict', encode, camelTo2)
import qualified Data.Aeson.Key
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens
import qualified Data.Set as Set
import Data.Aeson.Types (parseEither, parseJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char (toLower, ord)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)
import GHC.Generics (Generic)

jsonOptions :: Int -> Options
jsonOptions count = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop count }

argumentOptions :: Int -> Options
argumentOptions count = (jsonOptions count)
  { rejectUnknownFields = True, allowOmittedFields = False }

camelOptions :: Int -> Options
camelOptions count = defaultOptions
  { fieldLabelModifier = lowerFirst . drop count }

data ToolResult = ToolResult {resultStatus :: Text, resultResult :: Text} deriving (Generic)
instance ToJSON ToolResult where toJSON = genericToJSON (jsonOptions 6)

resultValue :: Text -> Value
resultValue outcome = toJSON (ToolResult "ok" outcome)

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (char : rest) = toLower char : rest

decodeValue :: FromJSON a => Text -> Value -> Result a
decodeValue code = either (const (Left (failure code))) Right . parseEither parseJSON

decodeBytes :: FromJSON a => Text -> ByteString -> Result a
decodeBytes code = either (const (Left (failure code))) Right . eitherDecodeStrict'

encodeBytes :: ToJSON a => a -> ByteString
encodeBytes = Lazy.toStrict . encode

encodeReference :: ToJSON a => a -> Text
encodeReference = Text.concatMap escape . Text.decodeUtf8 . encodeBytes
  where
  escape char
    | code > 0xffff = unicode high <> unicode low
    | code > 127 || char `elem` ("<>&" :: String) = unicode code
    | otherwise = Text.singleton char
    where code = ord char; adjusted = code - 0x10000; high = 0xd800 + adjusted `div` 0x400; low = 0xdc00 + adjusted `mod` 0x400
  unicode code = "\\u" <> Text.justifyRight 4 '0' (Text.pack (showHex code ""))

decodeUnique :: FromJSON a => Text -> ByteString -> Result a
decodeUnique = decodeUniqueNumbers (const True)

decodeUniqueIntegers :: FromJSON a => Text -> ByteString -> Result a
decodeUniqueIntegers = decodeUniqueNumbers integer
  where
    integer :: Number -> Bool
    integer (NumInteger _) = True
    integer _ = False

decodeUniqueNumbers :: FromJSON a => (Number -> Bool) -> Text -> ByteString -> Result a
decodeUniqueNumbers accepted code bytes = either (const (Left (failure code))) (const (decodeBytes code bytes)) checked
  where
    checked = uniqueTokens accepted (bsToTokens bytes)

uniqueTokens :: (Number -> Bool) -> Tokens k e -> Either () k
uniqueTokens _ (TkLit _ rest) = Right rest
uniqueTokens _ (TkText _ rest) = Right rest
uniqueTokens accepted (TkNumber number rest)
  | accepted number = Right rest
  | otherwise = Left ()
uniqueTokens accepted (TkArrayOpen items) = uniqueArray accepted items
uniqueTokens accepted (TkRecordOpen fields) = uniqueRecord accepted Set.empty fields
uniqueTokens _ (TkErr _) = Left ()

uniqueArray :: (Number -> Bool) -> TkArray k e -> Either () k
uniqueArray _ (TkArrayEnd rest) = Right rest
uniqueArray accepted (TkItem item) = uniqueTokens accepted item >>= uniqueArray accepted
uniqueArray _ (TkArrayErr _) = Left ()

uniqueRecord :: (Number -> Bool) -> Set.Set Data.Aeson.Key.Key -> TkRecord k e -> Either () k
uniqueRecord _ _ (TkRecordEnd rest) = Right rest
uniqueRecord accepted seen (TkPair key value)
  | Set.member key seen = Left ()
  | otherwise = uniqueTokens accepted value >>= uniqueRecord accepted (Set.insert key seen)
uniqueRecord _ _ (TkRecordErr _) = Left ()
