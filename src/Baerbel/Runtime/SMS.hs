module Baerbel.Runtime.SMS
  ( SMSRules(..), SMSCredentials(..), Delivery(..), Kind(..), Receipt(..)
  , SMS, boundedSMS, classify, withSMS, withAccountSMS
  ) where

import Baerbel.Core.SMS
    ( Receipt(..),
      Kind(..),
      Delivery(..),
      SMSCredentials(..),
      SMSRules(..),
      deliveryText,
      kindText,
      boundedSMS,
      classify,
      resumeReservation,
      smsRequest,
      savedReceipt )
import Baerbel.Runtime.Error ( Result )
import Baerbel.Runtime.HTTP ( RawSend )
import Baerbel.Runtime.Storage ( AccountStore(..), databaseResult, withDatabase, withAccountDatabase )
import Control.Concurrent.MVar ( newMVar, withMVar )
import Control.Monad ( unless )
import Crypto.Hash ( Digest, SHA256, hash )
import Data.Text ( Text )
import Database.SQLite.Simple
    ( changes,
      execute,
      execute_,
      query,
      query_,
      withTransaction,
      Only(..),
      Connection )
import qualified Data.Text as Text ( pack )
import qualified Data.Text.Encoding as Text ( encodeUtf8 )

type SMS = Text -> Text -> Kind -> IO Receipt

withSMS :: SMSRules -> SMSCredentials -> RawSend -> FilePath -> (SMS -> IO (Result a)) -> IO (Result a)
withSMS rules credentials sender path action = withDatabase "sms_state_unavailable" path opened
  where
  opened connection = do
    initialize connection
    withReceipts rules credentials sender (localReceipts connection) action

withAccountSMS :: SMSRules -> SMSCredentials -> RawSend -> AccountStore -> (SMS -> IO (Result a)) -> IO (Result a)
withAccountSMS rules credentials sender store action = withAccountDatabase "sms_state_unavailable" store open
  where
    open connection = withReceipts rules credentials sender (scopedReceipts connection (accountStoreId store)) action

withReceipts :: SMSRules -> SMSCredentials -> RawSend -> SMSReceipts -> (SMS -> IO (Result a)) -> IO (Result a)
withReceipts rules credentials sender receipts action = do
  gate <- newMVar ()
  let db operation = withMVar gate (const (databaseResult "sms_state_unavailable" operation))
      reserve ident text kind = db (reserveSMS receipts ident text kind)
      remember ident status = db (writeSMS receipts ident (deliveryText status))
  action (deliver rules credentials sender reserve remember)

initialize :: Connection -> IO ()
initialize connection = do
  execute_ connection "PRAGMA synchronous=FULL"
  execute_ connection "CREATE TABLE IF NOT EXISTS sms_sends (request_id TEXT PRIMARY KEY, fingerprint TEXT NOT NULL, status TEXT NOT NULL, created_at TEXT NOT NULL, kind TEXT NOT NULL DEFAULT 'notification', fallback INTEGER NOT NULL DEFAULT 0)"
  columns <- query_ connection "SELECT name FROM pragma_table_info('sms_sends')" :: IO [Only Text]
  let names = map fromOnly columns
  unless ("kind" `elem` names) (execute_ connection "ALTER TABLE sms_sends ADD COLUMN kind TEXT NOT NULL DEFAULT 'notification'")
  unless ("fallback" `elem` names) (execute_ connection "ALTER TABLE sms_sends ADD COLUMN fallback INTEGER NOT NULL DEFAULT 0")

data SMSReceipts = SMSReceipts
  { smsConnection :: Connection
  , insertSMS :: Text -> Text -> Text -> IO ()
  , readSMS :: Text -> IO [(Text, Text, Int)]
  , fallbackSMS :: Text -> Text -> IO ()
  , writeSMS :: Text -> Text -> IO () }

localReceipts :: Connection -> SMSReceipts
localReceipts connection = SMSReceipts connection insert readPrevious fallback update
  where
    insert ident digest kind = execute connection
      "INSERT OR IGNORE INTO sms_sends VALUES (?, ?, 'uncertain', strftime('%Y-%m-%dT%H:%M:%fZ','now'), ?, 0)" (ident, digest, kind)
    readPrevious ident = query connection "SELECT status, kind, fallback FROM sms_sends WHERE request_id=?" (Only ident)
    fallback ident digest = execute connection
      "UPDATE sms_sends SET status='uncertain', kind='outcome', fallback=1, fingerprint=? WHERE request_id=?" (digest, ident)
    update ident status = execute connection "UPDATE sms_sends SET status=? WHERE request_id=?" (status, ident)

scopedReceipts :: Connection -> Text -> SMSReceipts
scopedReceipts connection user = SMSReceipts connection insert readPrevious fallback update
  where
    insert ident digest kind = execute connection
      "INSERT OR IGNORE INTO sms_receipts VALUES (?, ?, ?, 'uncertain', strftime('%Y-%m-%dT%H:%M:%fZ','now'), ?, 0)" (user, ident, digest, kind)
    readPrevious ident = query connection "SELECT status, kind, fallback FROM sms_receipts WHERE user_id=? AND request_id=?" (user, ident)
    fallback ident digest = execute connection
      "UPDATE sms_receipts SET status='uncertain', kind='outcome', fallback=1, fingerprint=? WHERE user_id=? AND request_id=?" (digest, user, ident)
    update ident status = execute connection "UPDATE sms_receipts SET status=? WHERE user_id=? AND request_id=?" (status, user, ident)

reserveSMS :: SMSReceipts -> Text -> Text -> Kind -> IO (Maybe Delivery)
reserveSMS receipts ident body kind = withTransaction connection reserve
  where
  connection = smsConnection receipts
  digest = Text.pack (show (hash (Text.encodeUtf8 body) :: Digest SHA256))
  reserve = do
    insertSMS receipts ident digest (kindText kind)
    inserted <- changes connection
    reserved inserted
  reserved count
    | count > 0 = pure Nothing
    | otherwise = previous
  previous = do
    rows <- readSMS receipts ident
    let delivery = resumeReservation kind rows
    case delivery of
      Nothing -> fallbackSMS receipts ident digest
      Just _ -> pure ()
    pure delivery

type Reserve = Text -> Text -> Kind -> IO (Result (Maybe Delivery))
type Remember = Text -> Delivery -> IO (Result ())

deliver :: SMSRules -> SMSCredentials -> RawSend -> Reserve -> Remember -> SMS
deliver rules credentials sender reserve remember ident text kind =
  either (const (pure (Receipt Failed Nothing (Just True)))) accepted (boundedSMS rules text)
  where
  accepted body = do
    reservation <- reserve ident body kind
    case reservation of
      Left _ -> pure (Receipt Failed Nothing (Just True))
      Right (Just status) -> pure (Receipt status (Just True) Nothing)
      Right Nothing -> sendReserved rules credentials sender remember ident body

sendReserved :: SMSRules -> SMSCredentials -> RawSend -> Remember -> Text -> Text -> IO Receipt
sendReserved rules credentials sender remember ident body = do
  let request = smsRequest rules credentials body
  response <- sender request
  let status = either (const Uncertain) (classify rules) response
  savedReceipt status <$> remember ident status
