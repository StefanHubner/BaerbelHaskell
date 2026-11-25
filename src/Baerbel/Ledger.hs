module Baerbel.Ledger (Ledger, withLedger, durable, lookupExisting, fingerprint) where

import Baerbel.Error (Failure (..), Result, failure)
import qualified Baerbel.Storage as Storage
import Baerbel.JSON (decodeBytes, encodeBytes, jsonOptions)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value (..), ToJSON(..), encode, genericToJSON)
import GHC.Generics (Generic)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Vector as Vector
import Database.SQLite.Simple

data Ledger = Ledger Connection (MVar ())

withLedger :: FilePath -> (Ledger -> IO (Result a)) -> IO (Result a)
withLedger path action = Storage.withDatabase "send_state_unavailable" path initialize
  where
  initialize connection = do
    execute_ connection "PRAGMA synchronous=FULL"
    execute_ connection "CREATE TABLE IF NOT EXISTS sends (request_id TEXT PRIMARY KEY, fingerprint TEXT NOT NULL, result TEXT NOT NULL, created_at TEXT NOT NULL)"
    gate <- newMVar ()
    action (Ledger connection gate)

databaseResult :: IO a -> IO (Result a)
databaseResult = Storage.databaseResult "send_state_unavailable"

durable :: Ledger -> Text -> Text -> Value -> IO (Result Value) -> IO (Result Value)
durable ledger ident name arguments operation = do
  reserved <- reserve ledger ident name arguments
  case reserved of
    Left err -> pure (Left err)
    Right (Just previous) -> pure (Right previous)
    Right Nothing -> deliver ledger ident operation

deliver :: Ledger -> Text -> IO (Result Value) -> IO (Result Value)
deliver ledger ident operation = do
  result <- operation
  let stored = either failureValue id result
  remembered <- remember ledger ident stored
  pure (either (const (Left (Failure "send_receipt_storage_failed" True))) (const result) remembered)

reserve :: Ledger -> Text -> Text -> Value -> IO (Result (Maybe Value))
reserve (Ledger connection gate) ident name arguments = withMVar gate reserveLocked
  where
  reserveLocked _ = do
    saved <- databaseResult (insertReservation connection ident digest)
    pure (saved >>= resolve digest)
  digest = fingerprint name arguments

insertReservation :: Connection -> Text -> Text -> IO [(Text, Text)]
insertReservation connection ident digest = do
  execute connection "INSERT OR IGNORE INTO sends VALUES (?, ?, ?, strftime('%Y-%m-%dT%H:%M:%fZ','now'))" (ident, digest, pending)
  inserted <- changes connection
  existing inserted
  where
  existing count
    | count > 0 = pure []
    | otherwise = query connection "SELECT fingerprint, result FROM sends WHERE request_id=?" (Only ident)
  pending = Data.Text.Encoding.decodeUtf8 (encodeBytes (failureValue (Failure "send_in_progress_or_interrupted" True)))

resolve :: Text -> [(Text, Text)] -> Result (Maybe Value)
resolve _ [] = Right Nothing
resolve digest ((saved, result) : _)
  | digest /= saved = Left (failure "request_id_reused_with_different_arguments")
  | otherwise = Just . duplicate <$> decodeBytes "invalid_saved_receipt" bytes
  where
  bytes = Data.Text.Encoding.encodeUtf8 result
  duplicate (Object fields) = Object (KeyMap.insert "duplicate" (Bool True) fields)
  duplicate value = value

remember :: Ledger -> Text -> Value -> IO (Result ())
remember (Ledger connection gate) ident result = withMVar gate update
  where
  update _ = databaseResult (execute connection "UPDATE sends SET result=? WHERE request_id=?" (bytes, ident))
  bytes = Data.Text.Encoding.decodeUtf8 (encodeBytes result)

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

lookupExisting :: Ledger -> Text -> Text -> Value -> IO (Result (Maybe Value))
lookupExisting (Ledger connection gate) ident name arguments = withMVar gate readExisting
  where
  digest = fingerprint name arguments
  readExisting _ = do
    stored <- databaseResult (query connection "SELECT fingerprint, result FROM sends WHERE request_id=?" (Only ident))
    pure (stored >>= resolve digest)
