module Baerbel.SMS
  ( SMSRules(..), SMSCredentials(..), Delivery(..), Kind(..), Receipt(..)
  , SMS, boundedSMS, classify, withSMS
  ) where

import Data.Bifunctor (second)
import Baerbel.Error (Result, failure)
import Baerbel.HTTP (Request(..), Body(..), RawSend, Response(..))
import Baerbel.JSON (jsonOptions)
import Baerbel.Storage (databaseResult, withDatabase)
import Control.Monad (unless)
import Control.Concurrent.MVar (newMVar, withMVar)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), Options(..), genericParseJSON, genericToJSON)
import qualified Data.ByteString.Char8 as Bytes
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.SQLite.Simple
import GHC.Generics (Generic)

data SMSRules = SMSRules
  { smsEndpoint :: Text, smsMaximumParts :: Int, smsMaximumUnits :: Int
  , smsMaximumResponse :: Int, smsTimeoutSeconds :: Int, smsTruncatedMarker :: Text
  , smsRejectedStatuses :: [Int] } deriving (Eq, Show, Generic)
instance FromJSON SMSRules where parseJSON = genericParseJSON (jsonOptions 3)
data SMSCredentials = SMSCredentials
  { smsUsername :: Text, smsPassword :: Text, smsRecipient :: Text }
data Delivery = Sent | Failed | Uncertain | Skipped deriving (Eq, Show)
data Kind = Notification | Message | Outcome deriving (Eq, Show)
data Receipt = Receipt
  { receiptStatus :: Delivery, receiptDuplicate :: Maybe Bool, receiptRetryable :: Maybe Bool }
  deriving (Eq, Show, Generic)
instance ToJSON Delivery where toJSON = String . deliveryText
instance ToJSON Receipt where toJSON = genericToJSON ((jsonOptions 7) {omitNothingFields = True})
type SMS = Text -> Text -> Kind -> IO Receipt

deliveryText :: Delivery -> Text
deliveryText Sent = "sent"
deliveryText Failed = "failed"
deliveryText Uncertain = "uncertain"
deliveryText Skipped = "skipped"

parseDelivery :: Text -> Delivery
parseDelivery "sent" = Sent
parseDelivery "failed" = Failed
parseDelivery "skipped" = Skipped
parseDelivery _ = Uncertain

kindText :: Kind -> Text
kindText Notification = "notification"
kindText Message = "message"
kindText Outcome = "outcome"

boundedSMS :: SMSRules -> Text -> Result Text
boundedSMS rules body
  | Text.null (Text.strip body) || not (Text.all allowed body) = Left (failure "invalid_sms_text")
  | units body <= smsMaximumUnits rules = Right body
  | otherwise = Right (prefix budget body <> marker)
  where
  marker = smsTruncatedMarker rules
  budget = smsMaximumUnits rules - units marker
  allowed char = (ord char >= 32 || char == '\n' || char == '\t')
    && not (ord char >= 127 && ord char <= 159) && not (ord char >= 0xd800 && ord char <= 0xdfff)

units :: Text -> Int
units = Text.foldl' (\total char -> total + cost char) 0

cost :: Char -> Int
cost char
  | ord char > 0xffff = 2
  | otherwise = 1

prefix :: Int -> Text -> Text
prefix remaining text = case Text.uncons text of
  Nothing -> ""
  Just (char, rest) | cost char <= remaining -> Text.cons char following
    where following = prefix (remaining - cost char) rest
  _ -> ""

classify :: SMSRules -> Response -> Delivery
classify rules response
  | successful && "OK:" `Bytes.isPrefixOf` body = Sent
  | successful && "ERR:" `Bytes.isPrefixOf` body || status `elem` smsRejectedStatuses rules = Failed
  | otherwise = Uncertain
  where
  status = responseStatus response
  successful = status >= 200 && status < 300
  body = Bytes.dropWhileEnd (`elem` [' ', '\n', '\r', '\t']) (Bytes.dropWhile (`elem` [' ', '\n', '\r', '\t']) (responseBody response))

withSMS :: SMSRules -> SMSCredentials -> RawSend -> FilePath -> (SMS -> IO (Result a)) -> IO (Result a)
withSMS rules credentials sender path action = withDatabase "sms_state_unavailable" path opened
  where
  opened connection = do
    initialize connection
    gate <- newMVar ()
    let reserve ident text kind = withMVar gate (const (databaseResult "sms_state_unavailable" (reserveSMS connection ident text kind)))
        remember ident status = withMVar gate (const (databaseResult "sms_state_unavailable" (rememberSMS connection ident status)))
    action (deliver rules credentials sender reserve remember)

initialize :: Connection -> IO ()
initialize connection = do
  execute_ connection "PRAGMA synchronous=FULL"
  execute_ connection "CREATE TABLE IF NOT EXISTS sms_sends (request_id TEXT PRIMARY KEY, fingerprint TEXT NOT NULL, status TEXT NOT NULL, created_at TEXT NOT NULL, kind TEXT NOT NULL DEFAULT 'notification', fallback INTEGER NOT NULL DEFAULT 0)"
  columns <- query_ connection "SELECT name FROM pragma_table_info('sms_sends')" :: IO [Only Text]
  let names = map fromOnly columns
  unless ("kind" `elem` names) (execute_ connection "ALTER TABLE sms_sends ADD COLUMN kind TEXT NOT NULL DEFAULT 'notification'")
  unless ("fallback" `elem` names) (execute_ connection "ALTER TABLE sms_sends ADD COLUMN fallback INTEGER NOT NULL DEFAULT 0")

reserveSMS :: Connection -> Text -> Text -> Kind -> IO (Maybe Delivery)
reserveSMS connection ident body kind = withTransaction connection reserve
  where
  digest = Text.pack (show (hash (Text.encodeUtf8 body) :: Digest SHA256))
  reserve = do
    execute connection "INSERT OR IGNORE INTO sms_sends VALUES (?, ?, 'uncertain', strftime('%Y-%m-%dT%H:%M:%fZ','now'), ?, 0)" (ident, digest, kindText kind)
    inserted <- changes connection
    reserved inserted
  reserved count
    | count > 0 = pure Nothing
    | otherwise = previous
  previous = do
    rows <- query connection "SELECT status, kind, fallback FROM sms_sends WHERE request_id=?" (Only ident)
    resumeReservation connection ident digest kind rows

resumeReservation :: Connection -> Text -> Text -> Kind -> [(Text, Text, Int)] -> IO (Maybe Delivery)
resumeReservation connection ident digest Outcome [("failed", "message", 0)] = do
  execute connection "UPDATE sms_sends SET status='uncertain', kind='outcome', fallback=1, fingerprint=? WHERE request_id=?" (digest, ident)
  pure Nothing
resumeReservation _ _ _ _ ((status, _, _) : _) = pure (Just (parseDelivery status))
resumeReservation _ _ _ _ [] = pure (Just Uncertain)

rememberSMS :: Connection -> Text -> Delivery -> IO ()
rememberSMS connection ident status = execute connection "UPDATE sms_sends SET status=? WHERE request_id=?" (deliveryText status, ident)

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
  response <- sender request
  let status = either (const Uncertain) (classify rules) response
  saved <- remember ident status
  pure (Receipt (either (const Uncertain) (const status) saved) Nothing Nothing)
  where
  form = map (second Text.encodeUtf8)
    [("username", smsUsername credentials), ("password", smsPassword credentials)
    , ("da", smsRecipient credentials), ("ud", body), ("private", "1")
    , ("limit", Text.pack (show (smsMaximumParts rules)))]
  request = Request "POST" (smsEndpoint rules) [] [] (FormBody form)
    (smsMaximumResponse rules) (smsTimeoutSeconds rules)
