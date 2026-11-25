module Baerbel.Core.MIME where

import Data.ByteString ( ByteString )
import Data.Text ( Text )
import Data.Time ( UTCTime, defaultTimeLocale, formatTime )
import qualified Data.ByteString.Lazy as Lazy ( fromStrict )
import qualified Data.Text.Lazy as LazyText ( fromStrict )
import qualified Network.Mail.Mime as MIME
    ( addAttachmentsBS, plainPart, Address(Address), Mail(Mail) )
import qualified Data.Text as Text ( pack )

data Envelope = Envelope
  { envelopeFrom :: Text, envelopeTo :: [Text], envelopeSubject :: Text
  , envelopeBody :: Text, envelopeDate :: UTCTime, envelopeMessageId :: Text
  , envelopeHeaders :: [(ByteString, Text)] }
data File = File {fileMime :: Text, fileName :: Text, fileBytes :: ByteString}

compose :: Envelope -> [File] -> MIME.Mail
compose envelope files = MIME.addAttachmentsBS attachments message
  where
    sender = MIME.Address Nothing (envelopeFrom envelope)
    recipients = map (MIME.Address Nothing) (envelopeTo envelope)
    date = Text.pack (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (envelopeDate envelope))
    headers = [("Subject", envelopeSubject envelope), ("Date", date), ("Message-ID", envelopeMessageId envelope)] <> envelopeHeaders envelope
    body = LazyText.fromStrict (envelopeBody envelope)
    message = MIME.Mail sender recipients [] [] headers [[MIME.plainPart body]]
    attachments = map attachment files
    attachment file = (fileMime file, fileName file, Lazy.fromStrict (fileBytes file))
