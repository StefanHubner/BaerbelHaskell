module Baerbel.Runtime.Ledger (Ledger, withLedger, durable, lookupExisting, fingerprint) where

import Baerbel.Core.JSON ( encodeBytes )
import Baerbel.Core.Ledger ( resolve, failureValue, fingerprint )
import Baerbel.Runtime.Error ( Result, Failure(Failure) )
import Control.Concurrent.MVar ( MVar, newMVar, withMVar )
import Data.Aeson ( Value )
import Data.Text ( Text )
import Database.SQLite.Simple
    ( Only(Only), changes, execute, execute_, query, Connection )
import qualified Data.Text.Encoding ( decodeUtf8 )
import qualified Baerbel.Runtime.Storage as Storage
    ( databaseResult, withDatabase )

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

remember :: Ledger -> Text -> Value -> IO (Result ())
remember (Ledger connection gate) ident result = withMVar gate update
  where
  update _ = databaseResult (execute connection "UPDATE sends SET result=? WHERE request_id=?" (bytes, ident))
  bytes = Data.Text.Encoding.decodeUtf8 (encodeBytes result)

lookupExisting :: Ledger -> Text -> Text -> Value -> IO (Result (Maybe Value))
lookupExisting (Ledger connection gate) ident name arguments = withMVar gate readExisting
  where
  digest = fingerprint name arguments
  readExisting _ = do
    stored <- databaseResult (query connection "SELECT fingerprint, result FROM sends WHERE request_id=?" (Only ident))
    pure (stored >>= resolve digest)
