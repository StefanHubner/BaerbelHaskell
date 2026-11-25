{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Gmail (module Baerbel.Core.Gmail, GmailRules(..), gmail, gmailSession, mail, sendEnvelope, decodeSummaryHeaders, bodyText) where

import Baerbel.Core.Error ( Result, Failure )
import Baerbel.Core.Gmail
import Baerbel.Core.HTTP ( Request(requestUrl, requestMethod) )
import Baerbel.Core.JSON ( decodeValue )
import Baerbel.Core.ProviderConfig ( GmailRules(..) )
import Baerbel.Runtime.Cache ( onceSuccess )
import Baerbel.Runtime.Effects ( continue )
import Baerbel.Runtime.Error ( ioResult )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Baerbel.Runtime.MIME ( Envelope(..), File, render )
import Baerbel.Runtime.MailContent ( decodeWords, decodeCharset )
import Baerbel.Runtime.Provider
    ( referenceSize,
      get,
      textParam,
      combine,
      Durable,
      Execute,
      boundedMap )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except
    ( ExceptT(..), except, runExceptT )
import Data.Aeson hiding (Result)
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import qualified Data.ByteString as Bytes

type Decode = ExceptT Failure IO

-- Provider records are decoded once at the external boundary.
decodeHeader :: GmailRules -> Bool -> Text -> Part -> IO (Result Text)
decodeHeader rules unique name part = continue decodeWords (rawHeader rules unique name part)

header :: GmailRules -> Bool -> Text -> Part -> Decode Text
header rules unique name = ExceptT . decodeHeader rules unique name

decodeReplyHeaders :: GmailRules -> Part -> IO ReplyHeaders
decodeReplyHeaders rules part = ReplyHeaders <$> readHeader "Reply-To" <*> readHeader "From"
  where readHeader name = decodeHeader rules True name part

decodeSummaryHeaders :: GmailRules -> Message -> IO SummaryHeaders
decodeSummaryHeaders rules message = SummaryHeaders <$> readHeader "From" <*> readHeader "To"
  <*> readHeader "Subject" <*> readHeader "Date" <*> decodeReplyHeaders rules part
  where
    part = fromMaybe emptyPart (messagePayload message)
    readHeader name = decodeHeader rules False name part

bodyText :: GmailRules -> Part -> IO (Result MessageBody)
bodyText rules root = fmap (fmap (renderBody rules)) (runExceptT (walkParts rules initial [(0, root)]))
  where initial = BodyState [] [] [] (gmailPartBytes rules) (gmailPartBytes rules) 0 False

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

walkContent :: GmailRules -> BodyState -> Int -> Part -> [(Int, Part)] -> Decode BodyState
walkContent rules state depth part rest = do
  let remaining = remainingBytes part state
  contentType <- lift (decodeHeader rules False "Content-Type" part)
  content <- except (partContent remaining part contentType)
  decoded <- lift (decodeContent content)
  let (updated, pending) = consumePart rules state depth part rest decoded
  walkParts rules updated pending

decodeContent :: BodyContent -> IO (Text, Int, Bool)
decodeContent (EmptyContent cut) = pure ("", 0, cut)
decodeContent (EncodedContent charset bytes cut) = do
  text <- decodeCharset charset bytes
  pure (text, Bytes.length bytes, cut)

messageValue :: GmailRules -> Message -> Decode (Value, Bool)
messageValue rules message = do
  let part = fromMaybe emptyPart (messagePayload message)
  body <- ExceptT (bodyText rules part)
  decoded <- lift (decodeSummaryHeaders rules message)
  headers <- except (summary rules message decoded)
  pure (messageContent headers body)

search :: GmailRules -> Send -> SearchArgs -> IO (Result Value)
search rules send args = do
  let request = searchRequest rules args
      parse value = decodeValue "gmail_invalid_response" value >>= searchPage args
  page <- requestJSON send request parse
  continue retrieve page
  where
    retrieve :: (Page, [MessageId]) -> IO (Result Value)
    retrieve (page, ids) = do
      messages <- boundedMap (gmailSearchConcurrency rules) (readSummary rules send) ids
      pure (sequence messages >>= searchResult rules page)

readSummary :: GmailRules -> Send -> MessageId -> IO (Result Summary)
readSummary rules send ident = runExceptT $ do
  let request = readMessage rules (midId ident) "metadata"
  message <- ExceptT (requestJSON send request (decodeValue "gmail_invalid_response"))
  decoded <- lift (decodeSummaryHeaders rules message)
  except (summary rules message decoded)

readMail :: GmailRules -> Send -> ReadArgs -> IO (Result Value)
readMail rules send args = runExceptT $ do
  let request = readMessage rules (readMessageId args) "full"
  message <- ExceptT (requestJSON send request (decodeValue "gmail_invalid_response"))
  (value, _) <- messageValue rules message
  let mark = toJSON (ContentMark "ok" True)
  pure (combine [mark, value])

readThread :: GmailRules -> Send -> ThreadArgs -> IO (Result Value)
readThread rules send args = runExceptT $ do
  let request = get (gmailHttp rules) url params
  thread <- ExceptT (requestJSON send request (decodeValue "gmail_invalid_response"))
  finish thread
  where
    ident = argThreadId args
    url = gmailBase rules <> "/threads/" <> ident
    params = [textParam "format" "full"]
    finish thread = do
      let messages = fromMaybe [] (threadMessages thread)
      ordered <- except (orderMessages messages)
      let selected = take (gmailThreadMessages rules) ordered
      limited <- collectMessages rules (gmailThreadBytes rules) selected
      pure (threadResult rules ident (length messages) limited)

collectMessages :: GmailRules -> Int -> [Message] -> Decode [(Value, Bool)]
collectMessages _ _ [] = pure []
collectMessages rules remaining (message : rest) = do
  item <- messageValue rules message
  advance item
  where
    advance item@(value, _)
      | size > remaining = pure []
      | otherwise = (item :) <$> collectMessages rules next rest
      where
        size = referenceSize value
        next = remaining - size

sendEnvelope :: GmailRules -> Send -> Envelope -> [File] -> Maybe Text -> IO (Result Value)
sendEnvelope rules send envelope files thread = do
  rendered <- ioResult "gmail_message_encoding_failed" (render envelope files)
  continue submit rendered
  where
    submit bytes = do
      let request = sendRequest rules thread bytes
      requestJSON send request (sendReceipt (envelopeTo envelope))

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

sendMail :: GmailRules -> Send -> IO UTCTime -> Text -> SendArgs -> IO (Result Value)
sendMail rules send clock requestId args = do
  let request = accountRequest rules
  sender <- requestJSON send request account
  now <- clock
  let operation origin = mail rules send origin (sendTo args) (sendSubject args) (sendBody args) now requestId Nothing
  continue operation sender

replyMail :: GmailRules -> Send -> IO UTCTime -> Text -> ReplyArgs -> IO (Result Value)
replyMail rules send clock requestId args = runExceptT $ do
  let request = readMessage rules (replyMessageId args) "full"
  parent <- ExceptT (requestJSON send request (decodeValue "gmail_invalid_response"))
  let part = fromMaybe emptyPart (messagePayload parent)
  replyHeaders <- lift (decodeReplyHeaders rules part)
  destination <- except (replyAddress replyHeaders)
  original <- header rules True "Subject" part
  subject <- except (replySubject rules original)
  let profileRequest = accountRequest rules
  sender <- ExceptT (requestJSON send profileRequest account)
  now <- lift clock
  ExceptT (mail rules send sender [destination] subject (replyBody args) now requestId (Just parent))

gmail :: GmailRules -> Send -> Durable -> IO UTCTime -> Execute
gmail rules send durable clock name arguments requestId = continue execute (parseRequest rules name arguments)
  where
    execute GetProfile = requestJSON send profileRequest (fmap profile . account)
    execute (SearchMail args) = search rules send args
    execute (ReadMail args) = readMail rules send args
    execute (ReadThread args) = readThread rules send args
    execute (SendMail args) = durable requestId name (toJSON args) (sendMail rules send clock requestId args)
    execute (ReplyMail args) = durable requestId name (toJSON args) (replyMail rules send clock requestId args)
    profileRequest = accountRequest rules
    profile value = toJSON (ProfileResult "ok" value "gmail_account_profile")


gmailSession :: GmailRules -> Send -> Durable -> IO UTCTime -> IO Execute
gmailSession rules send durable clock = do
  let profileRequest = accountRequest rules
  cached <- onceSuccess (requestJSON send profileRequest account)
  let scoped request
        | requestMethod request == "GET" && requestUrl request == gmailBase rules <> "/profile" = fmap (fmap (toJSON . Account)) cached
        | otherwise = send request
  pure (gmail rules scoped durable clock)
