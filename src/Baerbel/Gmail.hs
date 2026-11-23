{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Gmail (GmailRules(..), gmail, gmailSession, mail, sendEnvelope, account, summary, bodyText) where

import Baerbel.Cache (onceSuccess)
import Baerbel.Error (Failure(..), Result, failure, ioResult)
import Baerbel.MIME (Envelope(..), File, render)
import Baerbel.MailContent (decodeWords, htmlText, decodeCharset)
import Baerbel.HTTP (Send, Request(..))
import Baerbel.GoogleValidation (address, identifier, textValue)
import Baerbel.JSON (argumentOptions, camelOptions, jsonOptions, decodeValue)
import Baerbel.Provider
import Baerbel.ProviderConfig (GmailRules(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64.URL as URL64
import Data.List (nub, sortOn)
import Data.Ord (Down(..))
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type Decode = ExceptT Failure IO

-- Provider records are decoded once at the external boundary.
data Header = Header {headerName :: Text, headerValue :: Text} deriving (Generic)
instance FromJSON Header where parseJSON = genericParseJSON (camelOptions 6)
data Content = Content
  { contentData :: Maybe Text, contentSize :: Maybe Int
  , contentAttachmentId :: Maybe Text } deriving (Generic)
instance FromJSON Content where parseJSON = genericParseJSON (camelOptions 7)
data Part = Part
  { partMimeType :: Maybe Text, partFilename :: Maybe Text, partBody :: Maybe Content
  , partHeaders :: Maybe [Header], partParts :: Maybe [Part] } deriving (Generic)
instance FromJSON Part where parseJSON = genericParseJSON (camelOptions 4)
data Message = Message
  { messageId :: Text, messageThreadId :: Text, messageSnippet :: Maybe Text
  , messageLabelIds :: Maybe [Text], messagePayload :: Maybe Part
  , messageInternalDate :: Maybe Text } deriving (Generic)
instance FromJSON Message where parseJSON = genericParseJSON (camelOptions 7)
newtype MessageId = MessageId {midId :: Text} deriving (Generic)
instance FromJSON MessageId where parseJSON = genericParseJSON (camelOptions 3)
data Page = Page {pageMessages :: Maybe [Value], pageNextPageToken :: Maybe Text} deriving (Generic)
instance FromJSON Page where
  parseJSON = withObject "message page" (\value -> Page <$> value .:! "messages" <*> value .:? "nextPageToken")
newtype Thread = Thread {threadMessages :: Maybe [Message]} deriving (Generic)
instance FromJSON Thread where
  parseJSON = withObject "thread" (\value -> Thread <$> value .:! "messages")
newtype Account = Account {accountEmailAddress :: Text} deriving (Generic)
instance FromJSON Account where parseJSON = genericParseJSON (camelOptions 7)
instance ToJSON Account where toJSON = genericToJSON (camelOptions 7)
data Receipt = Receipt {receiptId :: Text, receiptThreadId :: Text} deriving (Generic)
instance FromJSON Receipt where parseJSON = genericParseJSON (camelOptions 7)

data SearchArgs = SearchArgs {searchQuery :: Text, searchLimit :: Int, searchPageToken :: Maybe Text} deriving (Generic)
instance FromJSON SearchArgs where parseJSON = genericParseJSON (argumentOptions 6)
newtype ReadArgs = ReadArgs {readMessageId :: Text} deriving (Generic)
instance FromJSON ReadArgs where parseJSON = genericParseJSON (argumentOptions 4)
newtype ThreadArgs = ThreadArgs {argThreadId :: Text} deriving (Generic)
instance FromJSON ThreadArgs where parseJSON = genericParseJSON (argumentOptions 3)
data SendArgs = SendArgs {sendTo :: [Text], sendSubject :: Text, sendBody :: Text} deriving (Generic)
instance FromJSON SendArgs where parseJSON = genericParseJSON (argumentOptions 4)
instance ToJSON SendArgs where toJSON = genericToJSON (jsonOptions 4)
data ReplyArgs = ReplyArgs {replyMessageId :: Text, replyBody :: Text} deriving (Generic)
instance FromJSON ReplyArgs where parseJSON = genericParseJSON (argumentOptions 5)
instance ToJSON ReplyArgs where toJSON = genericToJSON (jsonOptions 5)

data Summary = Summary
  { summaryMessageId :: Text, summaryThreadId :: Text, summaryFrom :: Text
  , summaryTo :: Text, summarySubject :: Text, summaryDate :: Text
  , summarySnippet :: Text, summaryUnread :: Bool, summaryReplyTo :: Maybe Text
  , summaryReplyToError :: Maybe Text } deriving (Eq, Show, Generic)
instance ToJSON Summary where toJSON = genericToJSON (jsonOptions 7)
data Attachment = Attachment {attachmentFilename :: Text, attachmentMimeType :: Text, attachmentSize :: Int} deriving (Eq, Show, Generic)
instance ToJSON Attachment where toJSON = genericToJSON (jsonOptions 10)
data MessageBody = MessageBody {bodyBody :: Text, bodyTruncated :: Bool, bodyAttachments :: [Attachment]} deriving (Generic)
instance ToJSON MessageBody where toJSON = genericToJSON (jsonOptions 4)
data ContentMark = ContentMark {markStatus :: Text, markUntrustedEmailContent :: Bool} deriving (Generic)
instance ToJSON ContentMark where toJSON = genericToJSON (jsonOptions 4)
data SearchResult = SearchResult
  { resultStatus :: Text, resultMessages :: [Summary], resultResultCount :: Int
  , resultHasMore :: Bool, resultNextPageToken :: Maybe Text, resultSummaryScope :: Text
  , resultUntrustedEmailContent :: Bool } deriving (Generic)
instance ToJSON SearchResult where toJSON = genericToJSON (jsonOptions 6)
data ThreadResult = ThreadResult
  { threadResultStatus :: Text, threadResultThreadId :: Text, threadResultMessages :: [Value]
  , threadResultHasMore :: Bool, threadResultTruncated :: Bool, threadResultSummaryScope :: Text
  , threadResultUntrustedEmailContent :: Bool } deriving (Generic)
instance ToJSON ThreadResult where toJSON = genericToJSON (jsonOptions 12)
data ProfileResult = ProfileResult {profileStatus :: Text, profileEmailAddress :: Text, profileSource :: Text} deriving (Generic)
instance ToJSON ProfileResult where toJSON = genericToJSON (jsonOptions 7)
data SendResource = SendResource {resourceRaw :: Text, resourceThreadId :: Maybe Text} deriving (Generic)
instance ToJSON SendResource where toJSON = genericToJSON ((camelOptions 8) {omitNothingFields = True})
data SendResult = SendResult
  { sentStatus :: Text, sentSent :: Bool, sentMessageId :: Text, sentThreadId :: Text
  , sentTo :: [Text], sentRetrySafe :: Bool } deriving (Generic)
instance ToJSON SendResult where toJSON = genericToJSON (jsonOptions 4)


data ReplyHeaders = ReplyHeaders (Result Text) (Result Text)
data SummaryHeaders = SummaryHeaders (Result Text) (Result Text) (Result Text) (Result Text) ReplyHeaders

decodeHeader :: GmailRules -> Bool -> Text -> Part -> IO (Result Text)
decodeHeader rules unique name part = either (pure . Left) decodeWords (rawHeader rules unique name part)

header :: GmailRules -> Bool -> Text -> Part -> Decode Text
header rules unique name = ExceptT . decodeHeader rules unique name

decodeReplyHeaders :: GmailRules -> Part -> IO ReplyHeaders
decodeReplyHeaders rules part = ReplyHeaders <$> readHeader "Reply-To" <*> readHeader "From"
  where readHeader name = decodeHeader rules True name part

decodeSummaryHeaders :: GmailRules -> Part -> IO SummaryHeaders
decodeSummaryHeaders rules part = SummaryHeaders <$> readHeader "From" <*> readHeader "To"
  <*> readHeader "Subject" <*> readHeader "Date" <*> decodeReplyHeaders rules part
  where readHeader name = decodeHeader rules False name part

rawHeader :: GmailRules -> Bool -> Text -> Part -> Result Text
rawHeader rules unique name part
  | unique && length values > 1 = Left (failure "ambiguous_message_header")
  | Text.length value > gmailHeaderChars rules || Text.any invalid value = Left (failure "invalid_message_header")
  | otherwise = Right value
  where
    headers = fromMaybe [] (partHeaders part)
    values = [headerValue item | item <- headers, Text.toCaseFold (headerName item) == Text.toCaseFold name]
    value = unfoldHeader (firstOr "" values)
    invalid char = char < ' ' && char /= '\t'

unfoldHeader :: Text -> Text
unfoldHeader value = case Text.splitOn "\n" value of
  [] -> ""
  first : rest -> foldl' append first rest
  where
    append prior next
      | " " `Text.isPrefixOf` next || "\t" `Text.isPrefixOf` next = Text.dropWhileEnd (== '\r') prior <> " " <> Text.dropWhile horizontal next
      | otherwise = prior <> "\n" <> next
    horizontal char = char == ' ' || char == '\t'

emptyPart :: Part
emptyPart = Part Nothing Nothing Nothing Nothing Nothing

replyAddress :: ReplyHeaders -> Result Text
replyAddress (ReplyHeaders reply sender) = reply >>= choose >>= validateReplyAddress
  where
    choose supplied | Text.null supplied = sender
                    | otherwise = Right supplied

validateReplyAddress :: Text -> Result Text
validateReplyAddress sender
  | Text.any (\char -> char < ' ' && char /= '\t') sender = Left (failure "invalid_reply_address_header")
  | Text.null (Text.strip sender) || recipientCount sender /= 1 = Left (failure "reply_requires_one_recipient")
  | otherwise = address (extractAddress sender)

recipientCount :: Text -> Int
recipientCount value = third (Text.foldl' count (False, False, 1) value)
  where
    count (quoted, escaped, total) char
      | escaped = (quoted, False, total)
      | char == '\\' && quoted = (quoted, True, total)
      | char == '"' = (not quoted, False, total)
      | char == ',' && not quoted = (quoted, False, total + 1)
      | otherwise = (quoted, False, total)
    third (_, _, total) = total

extractAddress :: Text -> Text
extractAddress source = Text.strip selected
  where
    angle = Text.drop 1 (snd (Text.breakOn "<" source))
    selected | Text.null angle = source
             | otherwise = fst (Text.breakOn ">" angle)

summary :: GmailRules -> Message -> IO (Result Summary)
summary rules message = fmap (summarize rules message) (decodeSummaryHeaders rules part)
  where part = fromMaybe emptyPart (messagePayload message)

summarize :: GmailRules -> Message -> SummaryHeaders -> Result Summary
summarize rules message (SummaryHeaders sender recipient subject date replyHeaders) = do
  ident <- identifier (messageId message)
  thread <- identifier (messageThreadId message)
  fromValue <- Text.take (gmailSenderChars rules) <$> sender
  toValue <- Text.take (gmailRecipientChars rules) <$> recipient
  subjectValue <- Text.take (gmailSubjectChars rules) <$> subject
  dateValue <- Text.take (gmailDateChars rules) <$> date
  Right (Summary ident thread fromValue toValue subjectValue dateValue snippet unread reply problem)
  where
    outcome = replyAddress replyHeaders
    reply = either (const Nothing) Just outcome
    problem = either (Just . failureCode) (const Nothing) outcome
    snippet = Text.take (gmailSnippetChars rules) (fromMaybe "" (messageSnippet message))
    unread = "UNREAD" `elem` fromMaybe [] (messageLabelIds message)

data BodyState = BodyState
  { bodyPlain :: [Text], bodyHtml :: [Text], bodyFiles :: [Attachment]
  , bodyRemainingPlain :: Int, bodyRemainingHtml :: Int, bodyVisited :: Int, bodyCut :: Bool }
data BodyContent = EmptyContent Bool | EncodedContent Text Bytes.ByteString Bool

bodyText :: GmailRules -> Part -> IO (Result MessageBody)
bodyText rules root = fmap (fmap (renderBody rules)) (runExceptT (walkParts rules initial [(0, root)]))
  where initial = BodyState [] [] [] (gmailPartBytes rules) (gmailPartBytes rules) 0 False

renderBody :: GmailRules -> BodyState -> MessageBody
renderBody rules state = MessageBody (Text.take (gmailBodyChars rules) text) truncated (reverse (bodyFiles state))
  where
    plain = Text.intercalate "\n" (reverse (bodyPlain state))
    html = htmlText (Text.intercalate "\n" (reverse (bodyHtml state)))
    chosen | Text.null (Text.strip plain) = html
           | otherwise = plain
    text = Text.strip (Text.replace "\NUL" "" chosen)
    truncated = bodyCut state || Text.length text > gmailBodyChars rules

-- The walker interprets codec effects; selection, budgets and rendering are pure.
walkParts :: GmailRules -> BodyState -> [(Int, Part)] -> Decode BodyState
walkParts _ state [] = pure state
walkParts rules state ((depth, part) : rest)
  | depth > gmailMaximumDepth rules || bodyVisited state >= gmailMaximumParts rules = walkParts rules skipped rest
  | otherwise = header rules False "Content-Disposition" part >>= follow
  where
    visited = state {bodyVisited = bodyVisited state + 1}
    skipped = visited {bodyCut = True}
    follow disposition
      | attached part disposition = walkParts rules (visited {bodyFiles = attachment rules part : bodyFiles state}) rest
      | otherwise = walkContent rules visited depth part rest

attached :: Part -> Text -> Bool
attached part disposition = not (Text.null (fromMaybe "" (partFilename part)))
  || "attachment" `Text.isPrefixOf` Text.toLower disposition

walkContent :: GmailRules -> BodyState -> Int -> Part -> [(Int, Part)] -> Decode BodyState
walkContent rules state depth part rest = do
  let remaining = remainingBytes part state
  contentType <- lift (decodeHeader rules False "Content-Type" part)
  content <- except (partContent remaining part contentType)
  decoded <- lift (decodeContent content)
  let (updated, pending) = consumePart rules state depth part rest decoded
  walkParts rules updated pending

remainingBytes :: Part -> BodyState -> Int
remainingBytes part state = case partMimeType part of
  Just "text/plain" -> bodyRemainingPlain state
  _ -> bodyRemainingHtml state

consumePart :: GmailRules -> BodyState -> Int -> Part -> [(Int, Part)] -> (Text, Int, Bool) -> (BodyState, [(Int, Part)])
consumePart rules state depth part rest (text, used, cut) = (updated, pending)
  where
    remaining = remainingBytes part state
    next = state {bodyCut = bodyCut state || cut || length children > gmailMaximumParts rules}
    updated = case partMimeType part of
      Just "text/plain" | present -> next {bodyPlain = text : bodyPlain state, bodyRemainingPlain = remaining - used}
      Just "text/html" | present -> next {bodyHtml = text : bodyHtml state, bodyRemainingHtml = remaining - used}
      _ -> next
    present = maybe False (maybe False (not . Text.null) . contentData) (partBody part)
    children = fromMaybe [] (partParts part)
    childDepth = depth + 1
    pending = map (childDepth,) (take (gmailMaximumParts rules) children) <> rest

attachment :: GmailRules -> Part -> Attachment
attachment rules part = Attachment name mime size
  where
    name = Text.take (gmailAttachmentNameChars rules) (fromMaybe "" (partFilename part))
    mime = Text.take (gmailMimeChars rules) (fromMaybe "" (partMimeType part))
    size = maybe 0 (fromMaybe 0 . contentSize) (partBody part)

partContent :: Int -> Part -> Result Text -> Result BodyContent
partContent remaining part decodedHeader
  | mime `notElem` ["text/plain", "text/html"] = Right (EmptyContent False)
  | Text.null encoded = Right (EmptyContent external)
  | otherwise = do
      contentType <- decodedHeader
      raw <- either (const (Left (failure "invalid_message_body"))) Right (URL64.decode padded)
      let decoded = Bytes.take remaining raw
          parameters = map (Text.strip . Text.toLower) (Text.splitOn ";" contentType)
          charset = firstOr "utf-8" [Text.dropAround (== '"') (Text.drop 8 item) | item <- parameters, "charset=" `Text.isPrefixOf` item]
      Right (EncodedContent charset decoded (Text.length encoded > cap))
  where
    mime = fromMaybe "" (partMimeType part)
    encoded = maybe "" (fromMaybe "" . contentData) (partBody part)
    external = maybe False (maybe False (not . Text.null) . contentAttachmentId) (partBody part)
    cap = ((remaining + 2) `div` 3) * 4
    bytes = Text.encodeUtf8 (Text.take cap encoded)
    padding = (4 - Bytes.length bytes `mod` 4) `mod` 4
    padded = bytes <> Bytes.replicate padding 61

decodeContent :: BodyContent -> IO (Text, Int, Bool)
decodeContent (EmptyContent cut) = pure ("", 0, cut)
decodeContent (EncodedContent charset bytes cut) = do
  text <- decodeCharset charset bytes
  pure (text, Bytes.length bytes, cut)

messageValue :: GmailRules -> Message -> Decode (Value, Bool)
messageValue rules message = do
  let part = fromMaybe emptyPart (messagePayload message)
  body <- ExceptT (bodyText rules part)
  headers <- ExceptT (summary rules message)
  pure (messageContent headers body)

messageContent :: Summary -> MessageBody -> (Value, Bool)
messageContent headers body = (combine [toJSON headers, toJSON body], bodyTruncated body)

account :: GmailRules -> Send -> IO (Result Text)
account rules send = do
  let url = gmailBase rules <> "/profile"
  response <- fetch "gmail_invalid_response" (get (gmailHttp rules) send url [])
  pure (response >>= address . accountEmailAddress)

readMessage :: GmailRules -> Send -> Text -> Text -> IO (Result Message)
readMessage rules send ident format = fetch "gmail_invalid_response" request
  where
    url = gmailBase rules <> "/messages/" <> ident
    params = [textParam "format" format] <> [textParam "metadataHeaders" name | format == "metadata", name <- gmailMetadataHeaders rules]
    request = get (gmailHttp rules) send url params

search :: GmailRules -> Send -> SearchArgs -> IO (Result Value)
search rules send args
  | searchLimit args < 1 || searchLimit args > gmailSearchLimit rules = pure (Left (failure "invalid_search_limit"))
  | otherwise = do
      page <- fetch "gmail_invalid_response" request
      either (pure . Left) complete page
  where
    url = gmailBase rules <> "/messages"
    count = Text.pack (show (searchLimit args))
    params = [textParam "q" (searchQuery args), textParam "maxResults" count]
      <> maybe [] (pure . textParam "pageToken") (searchPageToken args)
    request = get (gmailHttp rules) send url params
    complete page = either (pure . Left) (summaries page) validatedIds
      where
        ids = take (searchLimit args) (fromMaybe [] (pageMessages page))
        validatedIds = traverse validateId ids
        validateId value = decodeValue "invalid_message_id" value >>= fmap MessageId . identifier . midId
    summaries page ids = do
      messages <- boundedMap (gmailSearchConcurrency rules) (readSummary rules send) ids
      pure (sequence messages >>= result page)
    result page items = do
      token <- traverse (textValue (gmailPageTokenChars rules) True) (pageNextPageToken page)
      Right (toJSON (SearchResult "ok" items (length items) (isJust token) token (gmailSearchScope rules) True))

readSummary :: GmailRules -> Send -> MessageId -> IO (Result Summary)
readSummary rules send ident = runExceptT (ExceptT operation >>= ExceptT . summary rules)
  where operation = readMessage rules send (midId ident) "metadata"

readMail :: GmailRules -> Send -> ReadArgs -> IO (Result Value)
readMail rules send args = runExceptT $ do
  message <- ExceptT (readMessage rules send (readMessageId args) "full")
  finish message
  where
    finish message = do
      (value, _) <- messageValue rules message
      let mark = toJSON (ContentMark "ok" True)
      pure (combine [mark, value])

internalDate :: Message -> Result Integer
internalDate message = maybe (Left (failure "gmail_invalid_response")) Right (messageInternalDate message >>= readMaybe . Text.unpack)

readThread :: GmailRules -> Send -> ThreadArgs -> IO (Result Value)
readThread rules send args = runExceptT $ do
  thread <- ExceptT (fetch "gmail_invalid_response" (get (gmailHttp rules) send url params))
  finish thread
  where
    ident = argThreadId args
    url = gmailBase rules <> "/threads/" <> ident
    params = [textParam "format" "full"]
    finish thread = do
      let messages = fromMaybe [] (threadMessages thread)
      dated <- except (traverse (\message -> (,) <$> internalDate message <*> pure message) messages)
      let ordered = map snd (sortOn (Down . fst) dated)
      limited <- collectMessages rules (gmailThreadBytes rules) (take (gmailThreadMessages rules) ordered)
      let more = length limited < length messages
          truncated = more || any snd limited
          output = ThreadResult "ok" ident (reverse (map fst limited)) more truncated (gmailThreadScope rules) True
      pure (toJSON output)

collectMessages :: GmailRules -> Int -> [Message] -> Decode [(Value, Bool)]
collectMessages _ _ [] = pure []
collectMessages rules remaining (message : rest) = do
  item <- messageValue rules message
  continue item
  where
    continue item@(value, _)
      | size > remaining = pure []
      | otherwise = (item :) <$> collectMessages rules next rest
      where
        size = referenceSize value
        next = remaining - size

sendEnvelope :: GmailRules -> Send -> Envelope -> [File] -> Maybe Text -> IO (Result Value)
sendEnvelope rules send envelope files thread = do
  rendered <- ioResult "gmail_message_encoding_failed" (render envelope files)
  either (pure . Left) submit rendered
  where
    submit bytes = do
      let encoded = Text.decodeUtf8 (URL64.encode bytes)
          resource = SendResource encoded thread
          url = gmailBase rules <> "/messages/send"
      response <- post (gmailHttp rules) send url [] resource
      pure (response >>= decodeReceipt)
    decodeReceipt value = either unknown receipt (decodeValue "gmail_send_response_missing_receipt" value)
    unknown issue = Left issue {failureUncertain = True}
    receipt value = case (identifier (receiptId value), identifier (receiptThreadId value)) of
      (Right ident, Right threadId) -> Right (toJSON (SendResult "ok" True ident threadId (envelopeTo envelope) False))
      _ -> Left (Failure "gmail_send_response_missing_receipt" True)

mail :: GmailRules -> Send -> Text -> [Text] -> Text -> Text -> UTCTime -> Text -> Maybe Message -> IO (Result Value)
mail rules send sender recipients subject body now requestId parent = runExceptT $ do
  context <- traverse (decodeThreading rules) parent
  ExceptT (deliver context)
  where
    ident = "<" <> requestId <> "@phone-assistant.local>"
    deliver context = sendEnvelope rules send envelope [] (fst <$> context)
      where envelope = Envelope sender recipients subject body now ident (maybe [] snd context)

decodeThreading :: GmailRules -> Message -> Decode (Text, [(Bytes.ByteString, Text)])
decodeThreading rules message = do
  values <- lift ((,) <$> readHeader "Message-ID" <*> readHeader "References")
  except (threading rules message values)
  where
    part = fromMaybe emptyPart (messagePayload message)
    readHeader name = decodeHeader rules True name part

threading :: GmailRules -> Message -> (Result Text, Result Text) -> Result (Text, [(Bytes.ByteString, Text)])
threading rules message (parentValue, referenceValue) = do
  parentId <- parentValue >>= checkParent
  ids <- referenceValue >>= checkReferences . Text.words
  thread <- identifier (messageThreadId message)
  let recent = drop (max 0 (length ids - gmailReferenceCount rules)) ids
      refs = Text.unwords (filter (/= parentId) recent <> [parentId])
  Right (thread, [("In-Reply-To", parentId), ("References", refs)])
  where
    checkParent ident
      | validMessageId ident && Text.length ident <= gmailThreadHeaderChars rules = Right ident
      | otherwise = Left (failure "reply_missing_or_invalid_message_id_header")
    checkReferences values
      | all validMessageId values = Right values
      | otherwise = Left (failure "reply_invalid_references_header")

validMessageId :: Text -> Bool
validMessageId value = "<" `Text.isPrefixOf` value && ">" `Text.isSuffixOf` value && validParts
  where
    inner = Text.dropEnd 1 (Text.drop 1 value)
    validPart part = not (Text.null part) && not (Text.any (\char -> isSpace char || char `elem` ("<>@" :: String)) part)
    validParts = case Text.splitOn "@" inner of
      [local, domain] -> validPart local && validPart domain
      _ -> False

sendMail :: GmailRules -> Send -> IO UTCTime -> Text -> SendArgs -> IO (Result Value)
sendMail rules send clock requestId args = do
  sender <- account rules send
  now <- clock
  let recipients = traverse address (nub (sendTo args))
      operation origin dests = mail rules send origin dests (sendSubject args) (sendBody args) now requestId Nothing
  either (pure . Left) id (operation <$> sender <*> recipients)

replyMail :: GmailRules -> Send -> IO UTCTime -> Text -> ReplyArgs -> IO (Result Value)
replyMail rules send clock requestId args = runExceptT $ do
  parent <- ExceptT (readMessage rules send (replyMessageId args) "full")
  let part = fromMaybe emptyPart (messagePayload parent)
  replyHeaders <- lift (decodeReplyHeaders rules part)
  destination <- except (replyAddress replyHeaders)
  original <- header rules True "Subject" part
  subject <- except (replySubject rules original)
  sender <- ExceptT (account rules send)
  now <- lift clock
  ExceptT (mail rules send sender [destination] subject (replyBody args) now requestId (Just parent))

replySubject :: GmailRules -> Text -> Result Text
replySubject rules original = addPrefix <$> textValue (gmailSendSubjectChars rules) True fallback
  where
    fallback | Text.null original = gmailEmptySubject rules
             | otherwise = original
    addPrefix subject | "re:" `Text.isPrefixOf` Text.toLower subject = subject
                      | otherwise = "Re: " <> subject

validateSend :: GmailRules -> SendArgs -> Result SendArgs
validateSend rules args
  | null (sendTo args) || length (sendTo args) > gmailSendRecipients rules = Left (failure "invalid_recipients")
  | otherwise = SendArgs <$> fmap nub (traverse address (sendTo args))
      <*> textValue (gmailSendSubjectChars rules) True (sendSubject args) <*> textValue (gmailSendBodyChars rules) False (sendBody args)

validateReply :: GmailRules -> ReplyArgs -> Result ReplyArgs
validateReply rules args = ReplyArgs <$> identifier (replyMessageId args) <*> textValue (gmailSendBodyChars rules) False (replyBody args)

validateSearch :: GmailRules -> SearchArgs -> Result SearchArgs
validateSearch rules args
  | searchLimit args < 1 || searchLimit args > gmailSearchLimit rules = Left (failure "invalid_search_limit")
  | otherwise = SearchArgs <$> textValue (gmailSearchChars rules) True (searchQuery args) <*> pure (searchLimit args)
      <*> traverse (textValue (gmailPageTokenChars rules) True) (searchPageToken args)

validated :: FromJSON a => (a -> Result a) -> (a -> IO (Result Value)) -> Value -> IO (Result Value)
validated validate operation arguments = either (pure . Left) operation parsed
  where parsed = decodeValue "invalid_tool_arguments" arguments >>= validate

gmail :: GmailRules -> Send -> Durable -> IO UTCTime -> Execute
gmail rules send durable clock name arguments requestId = case name of
  "gmail_profile" | arguments == Object mempty -> fmap (fmap profile) (account rules send)
                  | otherwise -> invalid
  "gmail_search" -> validated (validateSearch rules) (search rules send) arguments
  "gmail_read" -> validated validateRead (readMail rules send) arguments
  "gmail_thread" -> validated validateThread (readThread rules send) arguments
  "gmail_send" -> validated (validateSend rules) reserveSend arguments
  "gmail_reply" -> validated (validateReply rules) reserveReply arguments
  _ -> pure (Left (failure "unsupported_gmail_tool"))
  where
    invalid = pure (Left (failure "invalid_tool_arguments"))
    validateRead args = ReadArgs <$> identifier (readMessageId args)
    validateThread args = ThreadArgs <$> identifier (argThreadId args)
    reserveSend args = durable requestId name (toJSON args) (sendMail rules send clock requestId args)
    reserveReply args = durable requestId name (toJSON args) (replyMail rules send clock requestId args)
    profile value = toJSON (ProfileResult "ok" value "gmail_account_profile")


gmailSession :: GmailRules -> Send -> Durable -> IO UTCTime -> IO Execute
gmailSession rules send durable clock = do
  cached <- onceSuccess (account rules send)
  let scoped request
        | requestMethod request == "GET" && requestUrl request == gmailBase rules <> "/profile" = fmap (fmap (toJSON . Account)) cached
        | otherwise = send request
  pure (gmail rules scoped durable clock)
