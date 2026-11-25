{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Provider
  ( Nullable(..), renamedOptions, referenceSize, referenceByteSize, truthy, optionalField, get, post, textParam
  , clean, firstOr, limitJson, combine ) where

import Baerbel.Core.ProviderConfig (HttpPolicy(..))
import Baerbel.Core.HTTP (Body(..), Request(..))
import Baerbel.Core.JSON (jsonOptions)
import Data.Aeson.Types (parseMaybe)
import Data.Aeson (Object, Options(..), FromJSON(..), ToJSON(..), Value(..), encode)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.Aeson.Key as Key
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as Encoding
import Data.Foldable (toList)
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

newtype Nullable a = Nullable {nullable :: Maybe a}

-- Keep the default omitField=False: Nothing must remain an explicit JSON null.
instance ToJSON a => ToJSON (Nullable a) where
  toJSON = toJSON . nullable
  toEncoding = toEncoding . nullable

renamedOptions :: Int -> [(String, String)] -> Options
renamedOptions prefix names = base {fieldLabelModifier = rename}
  where
    base = jsonOptions prefix
    rename name = fromMaybe (fieldLabelModifier base name) (lookup name names)

referenceSize :: ToJSON a => a -> Int
referenceSize = jsonSize . toJSON

referenceByteSize :: ToJSON a => a -> Int
referenceByteSize value = referenceSize value + bytes - characters
  where
    encoded = Lazy.toStrict (encode value)
    bytes = fromIntegral (Lazy.length (encode value))
    characters = Text.length (Encoding.decodeUtf8 encoded)

jsonSize :: Value -> Int
jsonSize Null = 4
jsonSize (Bool True) = 4
jsonSize (Bool False) = 5
jsonSize (String value) = 2 + Text.foldl' (\size char -> size + escapedLength char) 0 value
jsonSize (Number value) = Text.length (Encoding.decodeUtf8 (Lazy.toStrict (encode value)))
jsonSize (Array values) = collectionSize (map jsonSize (toList values))
jsonSize (Object values) = collectionSize (map memberSize (KeyMap.toList values))
  where memberSize (key, value) = jsonSize (String (Key.toText key)) + 2 + jsonSize value

collectionSize :: [Int] -> Int
collectionSize values = 2 + sum values + max 0 (length values - 1) * 2

escapedLength :: Char -> Int
escapedLength char
  | char `elem` ("\"\\\b\f\n\r\t" :: String) = 2
  | ord char < 32 = 6
  | otherwise = 1

get :: HttpPolicy -> Text -> [(ByteString, Maybe ByteString)] -> Request
get policy url query = Request "GET" url [] query EmptyBody (httpResponseBytes policy) (httpTimeoutSeconds policy)

post :: ToJSON a => HttpPolicy -> Text -> [(ByteString, Maybe ByteString)] -> a -> Request
post policy url query body = Request "POST" url [] query (JsonBody resource) (httpResponseBytes policy) (httpTimeoutSeconds policy)
  where resource = toJSON body

textParam :: ByteString -> Text -> (ByteString, Maybe ByteString)
textParam key value = (key, Just encoded)
  where encoded = Text.encodeUtf8 value

clean :: Int -> Text -> Text
clean cap = Text.take cap . Text.filter visible
  where visible char = char >= ' ' && char /= '\DEL'

firstOr :: a -> [a] -> a
firstOr fallback values = case values of
  [] -> fallback
  value : _ -> value

limitJson :: ToJSON a => Int -> Int -> [a] -> ([a], Bool)
limitJson count cap = go 0 []
  where
    go _ result [] = (reverse result, False)
    go size result (value : rest)
      | length result >= count || total > cap = (reverse result, True)
      | otherwise = go total (value : result) rest
      where total = size + referenceSize value

combine :: [Value] -> Value
combine values = Object (foldMap fields values)
  where
    fields (Object value) = value
    fields _ = KeyMap.empty


truthy :: Value -> Bool
truthy Null = False
truthy (Bool value) = value
truthy (Number value) = value /= 0
truthy (String value) = not (Text.null value)
truthy (Array value) = not (null value)
truthy (Object value) = not (KeyMap.null value)


optionalField :: FromJSON a => Key.Key -> Object -> Maybe a
optionalField key value = KeyMap.lookup key value >>= parseMaybe parseJSON
