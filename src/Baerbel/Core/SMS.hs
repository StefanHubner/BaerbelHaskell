module Baerbel.Core.SMS where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.HTTP ( Request(..), Body(..), Response(..) )
import Baerbel.Core.JSON ( jsonOptions )
import Data.Aeson
    ( FromJSON(..),
      ToJSON(..),
      Value(String),
      Options(..),
      genericParseJSON,
      genericToJSON )
import Data.Bifunctor ( second )
import Data.Char ( ord )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Char8 as Bytes
    ( isPrefixOf, dropWhile, dropWhileEnd )
import qualified Data.Text as Text
    ( all, cons, foldl', null, strip, uncons, pack )
import qualified Data.Text.Encoding as Text ( encodeUtf8 )

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

resumeReservation :: Kind -> [(Text, Text, Int)] -> Maybe Delivery
resumeReservation Outcome [("failed", "message", 0)] = Nothing
resumeReservation _ ((status, _, _) : _) = Just (parseDelivery status)
resumeReservation _ [] = Just Uncertain

smsRequest :: SMSRules -> SMSCredentials -> Text -> Request
smsRequest rules credentials body = Request "POST" (smsEndpoint rules) [] [] (FormBody form)
  (smsMaximumResponse rules) (smsTimeoutSeconds rules)
  where
    form = map (second Text.encodeUtf8)
      [("username", smsUsername credentials), ("password", smsPassword credentials)
      , ("da", smsRecipient credentials), ("ud", body), ("private", "1")
      , ("limit", Text.pack (show (smsMaximumParts rules)))]

savedReceipt :: Delivery -> Result () -> Receipt
savedReceipt status result = Receipt confirmed Nothing Nothing
  where confirmed = either (const Uncertain) (const status) result
