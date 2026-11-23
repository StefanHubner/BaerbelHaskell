module Baerbel.NoteMail (NoteMailRules(..), NoteDelivery, noteSession, noteSessionWith) where

import Baerbel.Cache (onceSuccess)
import Baerbel.Encoder (Encoding, encodeAudio)
import Baerbel.Error (Failure(..), Result)
import Baerbel.Gmail (account, sendEnvelope)
import Baerbel.HTTP (Send)
import Baerbel.JSON (jsonOptions)
import Baerbel.MIME (Envelope(..), File(..))
import Baerbel.Provider (Durable)
import Baerbel.ProviderConfig (GmailRules(..), HttpPolicy)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, genericParseJSON, genericToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)

data NoteMailRules = NoteMailRules
  { noteMailEncoding :: Encoding, noteMailSubject :: Text
  , noteMailBodyPrefix :: Text, noteMailBodySuffix :: Text
  , noteMailFilenamePattern :: String, noteMailMime :: Text, noteMailHttp :: HttpPolicy } deriving (Generic)
instance FromJSON NoteMailRules where parseJSON = genericParseJSON (jsonOptions 8)
data Payload = Payload {payloadTranscript :: Text, payloadAudioSha256 :: Text} deriving (Generic)
instance ToJSON Payload where toJSON = genericToJSON (jsonOptions 7)
data Receipt = Receipt {receiptStatus :: Text, receiptResult :: Text} deriving (Generic)
instance ToJSON Receipt where toJSON = genericToJSON (jsonOptions 7)

type NoteDelivery = Text -> Text -> ByteString -> IO (Result Value)

noteSession :: NoteMailRules -> GmailRules -> Send -> Durable -> IO UTCTime -> IO NoteDelivery
noteSession rules = noteSessionWith (encodeAudio (noteMailEncoding rules)) rules

noteSessionWith :: (ByteString -> IO (Result ByteString)) -> NoteMailRules -> GmailRules -> Send -> Durable -> IO UTCTime -> IO NoteDelivery
noteSessionWith encode rules gmailRules send durable clock = do
  owner <- onceSuccess (account configured send)
  pure (noteDelivery encode owner rules configured send durable clock)
  where configured = gmailRules {gmailHttp = noteMailHttp rules}

noteDelivery :: (ByteString -> IO (Result ByteString)) -> IO (Result Text) -> NoteMailRules -> GmailRules -> Send -> Durable -> IO UTCTime -> NoteDelivery
noteDelivery encode owner rules gmailRules send durable clock ident transcript pcm =
  fmap normalize (durable requestId "owner_note" payload operation)
  where
    requestId = "owner-note:" <> ident
    digest = hash pcm :: Digest SHA256
    payload = toJSON (Payload transcript (Text.pack (show digest)))
    operation = normalize <$> prepare
    prepare = do
      encoded <- encode pcm
      either (pure . Left) (deliver owner rules gmailRules send clock ident transcript) encoded
    normalize = either (Right . toJSON . mailFailure) Right

deliver :: IO (Result Text) -> NoteMailRules -> GmailRules -> Send -> IO UTCTime -> Text -> Text -> ByteString -> IO (Result Value)
deliver owner rules gmailRules send clock ident transcript audio = do
  sender <- owner
  now <- clock
  either (pure . Left) (submit now) sender
  where
    submit now origin = do
      let body = noteMailBodyPrefix rules <> transcript <> noteMailBodySuffix rules
          messageId = "<owner-note-" <> ident <> "@phone-assistant.local>"
          envelope = Envelope origin [origin] (noteMailSubject rules) body now messageId []
          name = Text.pack (formatTime defaultTimeLocale (noteMailFilenamePattern rules) now)
          file = File (noteMailMime rules) name audio
      result <- sendEnvelope gmailRules send envelope [file] Nothing
      pure (toJSON (Receipt "ok" "sent") <$ result)

data MailFailure = MailFailure {mailFailureStatus :: Text, mailFailureError :: Text} deriving (Generic)
instance ToJSON MailFailure where toJSON = genericToJSON (jsonOptions 11)

mailFailure :: Failure -> MailFailure
mailFailure issue
  | failureUncertain issue = MailFailure "unknown" "note_send_outcome_unknown_do_not_retry"
  | otherwise = MailFailure "error" "note_send_failed"
