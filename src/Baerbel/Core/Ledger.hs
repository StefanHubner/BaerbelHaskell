module Baerbel.Core.Ledger where

import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Core.JSON ( jsonOptions, decodeBytes )
import Crypto.Hash ( Digest, SHA256, hash )
import Data.Aeson ( Value(..), ToJSON(..), encode, genericToJSON )
import Data.ByteString ( ByteString )
import Data.List ( sortOn )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Char8 as BS ( intercalate )
import qualified Data.Aeson.Key as Key ( toText )
import qualified Data.Aeson.KeyMap as KeyMap ( insert, toList )
import qualified Data.ByteString.Lazy as Lazy ( toStrict )
import qualified Data.Text as Text ( pack )
import qualified Data.Vector as Vector ( fromList, toList )

resolve :: Text -> [(Text, Text)] -> Result (Maybe Value)
resolve _ [] = Right Nothing
resolve digest ((saved, result) : _)
  | digest /= saved = Left (failure "request_id_reused_with_different_arguments")
  | otherwise = Just . duplicate <$> decodeBytes "invalid_saved_receipt" bytes
  where
  bytes = Data.Text.Encoding.encodeUtf8 result
  duplicate (Object fields) = Object (KeyMap.insert "duplicate" (Bool True) fields)
  duplicate value = value

failureValue :: Failure -> Value
failureValue err = toJSON (FailureReceipt status (failureCode err) False)
  where
    status
      | failureUncertain err = "unknown"
      | otherwise = "error"

data FailureReceipt = FailureReceipt
  { receiptStatus :: Text, receiptError :: Text, receiptRetrySafe :: Bool } deriving (Generic)
instance ToJSON FailureReceipt where toJSON = genericToJSON (jsonOptions 7)

fingerprint :: Text -> Value -> Text
fingerprint name arguments = Text.pack (show digest)
  where
  payload = Array (Vector.fromList [String name, arguments])
  digest = hash (canonical payload) :: Digest SHA256

-- Match Python's persisted sort_keys=True, ensure_ascii=False JSON fingerprint.
canonical :: Value -> ByteString
canonical (Object fields) = "{" <> BS.intercalate ", " members <> "}"
  where
  pairs = sortOn (Key.toText . fst) (KeyMap.toList fields)
  member (key, value) = canonical (String (Key.toText key)) <> ": " <> canonical value
  members = map member pairs
canonical (Array values) = "[" <> BS.intercalate ", " elements <> "]"
  where elements = map canonical (Vector.toList values)
canonical value = Lazy.toStrict (encode value)
