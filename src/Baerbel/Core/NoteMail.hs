module Baerbel.Core.NoteMail where

import Baerbel.Core.Encoder ( Encoding )
import Baerbel.Core.Error ( Failure(..), Result )
import Baerbel.Core.JSON ( jsonOptions )
import Baerbel.Core.MIME ( Envelope(..), File(..) )
import Baerbel.Core.ProviderConfig ( HttpPolicy )
import Crypto.Hash ( Digest, SHA256, hash )
import Data.Aeson
    ( Value,
      FromJSON(..),
      ToJSON(..),
      genericParseJSON,
      genericToJSON )
import Data.ByteString ( ByteString )
import Data.Text ( Text )
import Data.Time ( UTCTime, defaultTimeLocale, formatTime )
import GHC.Generics ( Generic )
import qualified Data.Text as Text ( pack )

data NoteMailRules = NoteMailRules
  { noteMailEncoding :: Encoding, noteMailSubject :: Text
  , noteMailBodyPrefix :: Text, noteMailBodySuffix :: Text
  , noteMailFilenamePattern :: String, noteMailMime :: Text, noteMailHttp :: HttpPolicy } deriving (Generic)
instance FromJSON NoteMailRules where parseJSON = genericParseJSON (jsonOptions 8)
data Payload = Payload {payloadTranscript :: Text, payloadAudioSha256 :: Text} deriving (Generic)
instance ToJSON Payload where toJSON = genericToJSON (jsonOptions 7)
data Receipt = Receipt {receiptStatus :: Text, receiptResult :: Text} deriving (Generic)
instance ToJSON Receipt where toJSON = genericToJSON (jsonOptions 7)

noteMessage :: NoteMailRules -> UTCTime -> Text -> Text -> Text -> ByteString -> (Envelope, File)
noteMessage rules now origin ident transcript audio = (envelope, file)
  where
    body = noteMailBodyPrefix rules <> transcript <> noteMailBodySuffix rules
    messageId = "<owner-note-" <> ident <> "@phone-assistant.local>"
    envelope = Envelope origin [origin] (noteMailSubject rules) body now messageId []
    name = Text.pack (formatTime defaultTimeLocale (noteMailFilenamePattern rules) now)
    file = File (noteMailMime rules) name audio

data MailFailure = MailFailure {mailFailureStatus :: Text, mailFailureError :: Text} deriving (Generic)
instance ToJSON MailFailure where toJSON = genericToJSON (jsonOptions 11)

mailFailure :: Failure -> MailFailure
mailFailure issue
  | failureUncertain issue = MailFailure "unknown" "note_send_outcome_unknown_do_not_retry"
  | otherwise = MailFailure "error" "note_send_failed"

notePayload :: Text -> ByteString -> Value
notePayload transcript pcm = toJSON (Payload transcript (Text.pack (show digest)))
  where digest = hash pcm :: Digest SHA256

normalizeReceipt :: Result Value -> Result Value
normalizeReceipt = either (Right . toJSON . mailFailure) Right
