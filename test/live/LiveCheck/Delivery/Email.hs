module LiveCheck.Delivery.Email (selfEmail) where

import Baerbel.Core.Config (Settings, rule)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.Gmail
import Baerbel.Core.HTTP (Request(..))
import Baerbel.Core.JSON (decodeValue, textField)
import Baerbel.Core.NoteMail (NoteMailRules(..))
import Baerbel.Core.Provider (get)
import Baerbel.Core.ProviderConfig (GmailRules(..))
import Baerbel.Core.Speech (audiblePCM)
import Baerbel.Runtime.Encoder (encodeAudio)
import Baerbel.Runtime.Gmail (decodeSummaryHeaders)
import Baerbel.Runtime.HTTP (Send, requestJSON, newRawSender)
import Baerbel.Runtime.Ledger (Ledger, durable)
import Baerbel.Runtime.NoteMail (noteSession)
import Control.Exception (mask)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson (Value, object, (.=))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import LiveCheck.Check (ensure, emit)
import LiveCheck.Config (SpeechSettings)
import LiveCheck.Conversation.Audio (synthesise, spoken)
import LiveCheck.Delivery.Common (freshId, withReceiptLedger)
import LiveCheck.Native (decoder)
import LiveCheck.Providers.Common (member)
import Network.HTTP.Types.URI (urlEncode)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Text.Encoding as Text

subject :: Text
subject = "Bärbel Haskell live check"

selfEmail :: SpeechSettings -> Settings -> Send -> IO (Result Value)
selfEmail speechRules settings sender = runExceptT $ do
  mailRules <- except (rule settings "notes" "mail")
  gmailRules <- except (rule settings "providers" "gmail")
  let labelled = mailRules {noteMailSubject = subject}
  pcm <- ExceptT (syntheticClip speechRules settings)
  ident <- lift freshId
  ExceptT (withReceiptLedger (sendAndVerify labelled gmailRules sender ident pcm))

syntheticClip :: SpeechSettings -> Settings -> IO (Result ByteString)
syntheticClip speechRules settings = runExceptT $ do
  rate <- except (rule settings "runtime" "input_rate" :: Result Int)
  raw <- lift newRawSender
  let text = "This is Bärbel's authorised Haskell voice note delivery check."
      speech = synthesise speechRules settings raw
  clip <- ExceptT (spoken settings speech text)
  let size = rate * 2 * 6
      silence = Bytes.replicate size 0
      bounded = Bytes.take size (clip <> silence)
  except (ensure "live_delivery_audio_silent" (audiblePCM bounded))
  pure bounded

sendAndVerify :: NoteMailRules -> GmailRules -> Send -> Text -> ByteString -> Ledger -> IO (Result Value)
sendAndVerify rules gmailRules sender ident pcm ledger = runExceptT $ do
  owner <- ExceptT (requestJSON sender (accountRequest gmailRules) account)
  captured <- lift (newIORef Nothing)
  let observed = captureReceipt gmailRules captured sender
  deliver <- lift (noteSession rules gmailRules observed (durable ledger) getCurrentTime)
  result <- ExceptT (deliver ident "Authorised synthetic Haskell delivery check. No action is needed." pcm)
  except (sent result)
  receipt <- lift (readIORef captured)
  messageIdValue <- except (sentIdentifier owner receipt)
  ExceptT (verifyMessage rules gmailRules sender owner messageIdValue)

captureReceipt :: GmailRules -> IORef (Maybe Value) -> Send -> Send
captureReceipt rules captured sender request = mask $ \restore -> do
  result <- restore (sender request)
  let record value = writeIORef captured (Just value)
  case (requestMethod request, requestUrl request == gmailBase rules <> "/messages/send") of
    ("POST", True) -> mapM_ record result
    _ -> pure ()
  pure result

sent :: Value -> Result ()
sent value = case (textField "status" value, textField "result" value) of
  (Just "ok", Just "sent") -> Right ()
  (Just "unknown", _) -> Left (Failure "live_email_outcome_unknown_do_not_retry" True)
  _ -> Left (failure "live_email_send_failed")

sentIdentifier :: Text -> Maybe Value -> Result Text
sentIdentifier owner receipt = do
  raw <- maybe (Left (Failure "live_email_receipt_missing_do_not_retry" True)) Right receipt
  normalized <- sendReceipt [owner] raw
  member "message_id" normalized

verifyMessage :: NoteMailRules -> GmailRules -> Send -> Text -> Text -> IO (Result Value)
verifyMessage rules gmailRules sender owner ident = runExceptT $ do
  message <- ExceptT (readSentMessage gmailRules sender ident 3)
  headers <- lift (decodeSummaryHeaders gmailRules message)
  attachmentPart <- except (validateMessage gmailRules owner ident message headers)
  mp3 <- ExceptT (attachmentBytes gmailRules sender ident attachmentPart)
  let encoding = decoder (noteMailEncoding rules)
  decoded <- ExceptT (encodeAudio encoding mp3)
  report <- except (emailReport message mp3 decoded)
  ExceptT (confirmInbox message report)

validateMessage :: GmailRules -> Text -> Text -> Message -> SummaryHeaders -> Result Part
validateMessage rules owner ident message headers = do
  metadata <- summary rules message headers
  recipient <- validateReplyAddress (summaryTo metadata)
  ensure "live_email_recipient_mismatch" (recipient == owner)
  ensure "live_email_readback_mismatch" (messageId message == ident && summarySubject metadata == subject)
  ensure "live_email_sent_label_missing" ("SENT" `elem` fromMaybe [] (messageLabelIds message))
  let root = fromMaybe emptyPart (messagePayload message)
      attachments = filter ((== Just "audio/mpeg") . partMimeType) (messageParts root)
  case attachments of
    [part] -> Right part
    _ -> Left (failure "live_email_attachment_missing")

messageParts :: Part -> [Part]
messageParts part = part : maybe [] (concatMap messageParts) (partParts part)

attachmentBytes :: GmailRules -> Send -> Text -> Part -> IO (Result ByteString)
attachmentBytes rules sender ident part = case partBody part of
  Just content | Just encoded <- contentData content -> pure (decodeAttachment content encoded)
  Just content | Just reference <- contentAttachmentId content -> requestAttachment rules sender ident reference
  _ -> pure (Left (failure "live_email_attachment_missing"))

requestAttachment :: GmailRules -> Send -> Text -> Text -> IO (Result ByteString)
requestAttachment rules sender ident reference = runExceptT $ do
  let suffix = Text.decodeUtf8 (urlEncode True (Text.encodeUtf8 reference))
      url = gmailBase rules <> "/messages/" <> ident <> "/attachments/" <> suffix
      request = get (gmailHttp rules) url []
  content <- ExceptT (requestJSON sender request (decodeValue "live_email_attachment_invalid"))
  encoded <- except (maybe (Left (failure "live_email_attachment_missing")) Right (contentData content))
  except (decodeAttachment content encoded)

decodeAttachment :: Content -> Text -> Result ByteString
decodeAttachment content text = do
  let encoded = Text.encodeUtf8 text
  bytes <- first (const (failure "live_email_attachment_invalid")) (Base64.decode encoded)
  ensure "live_email_attachment_size_mismatch" (contentSize content == Just (Bytes.length bytes))
  Right bytes

emailReport :: Message -> ByteString -> ByteString -> Result Value
emailReport message mp3 decoded = do
  let seconds = fromIntegral (Bytes.length decoded) / (16000 :: Double)
      inbox = inInbox message
      status | inbox = "ok" :: Text
             | otherwise = "inbox_unconfirmed"
  ensure "live_email_attachment_audio_invalid" (audiblePCM decoded && abs (seconds - 6) < 0.25)
  Right (object ["status" .= status, "send_accepted" .= True, "sent_copy_verified" .= True,
    "recipient_is_self" .= True, "own_inbox_confirmed" .= inbox, "attachment_bytes" .= Bytes.length mp3,
    "attachment_mime_verified" .= True, "decoded_seconds" .= seconds])

readSentMessage :: GmailRules -> Send -> Text -> Int -> IO (Result Message)
readSentMessage rules sender ident attempts = do
  let request = readMessage rules ident "full"
  result <- requestJSON sender request (decodeValue "live_email_readback_invalid")
  case result of
    Right message | inInbox message -> pure result
    Right _ | attempts > 1 -> retry
    Left issue | failureCode issue == "http_404" && attempts > 1 -> retry
    _ -> pure result
  where retry = threadDelay 1000000 >> readSentMessage rules sender ident (attempts - 1)

confirmInbox :: Message -> Value -> IO (Result Value)
confirmInbox message report = do
  emit (object ["email_delivery" .= report])
  pure (report <$ ensure "live_email_send_accepted_inbox_not_confirmed_do_not_retry" (inInbox message))

inInbox :: Message -> Bool
inInbox message = "INBOX" `elem` fromMaybe [] (messageLabelIds message)
