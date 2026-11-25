
module Baerbel.Runtime.NoteMail
  ( module Core, NoteDelivery, noteSession, noteSessionWith ) where

import Baerbel.Core.NoteMail as Core
import Baerbel.Runtime.Cache (onceSuccess)
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.Encoder (encodeAudio)
import Baerbel.Core.Error (Result)
import Baerbel.Runtime.Gmail (account, accountRequest, sendEnvelope)
import Baerbel.Runtime.HTTP (Send, requestJSON)
import Baerbel.Runtime.Provider (Durable)
import Baerbel.Core.ProviderConfig (GmailRules(..))
import Data.Aeson (ToJSON(..), Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

type NoteDelivery = Text -> Text -> ByteString -> IO (Result Value)

noteSession :: NoteMailRules -> GmailRules -> Send -> Durable -> IO UTCTime -> IO NoteDelivery
noteSession rules = noteSessionWith (encodeAudio (noteMailEncoding rules)) rules

noteSessionWith :: (ByteString -> IO (Result ByteString)) -> NoteMailRules -> GmailRules -> Send -> Durable -> IO UTCTime -> IO NoteDelivery
noteSessionWith encode rules gmailRules send durable clock = do
  owner <- onceSuccess (requestJSON send request account)
  pure (noteDelivery encode owner rules configured send durable clock)
  where
    configured = gmailRules {gmailHttp = noteMailHttp rules}
    request = accountRequest configured

noteDelivery :: (ByteString -> IO (Result ByteString)) -> IO (Result Text) -> NoteMailRules -> GmailRules -> Send -> Durable -> IO UTCTime -> NoteDelivery
noteDelivery encode owner rules gmailRules send durable clock ident transcript pcm =
  fmap normalizeReceipt (durable requestId "owner_note" payload operation)
  where
    requestId = "owner-note:" <> ident
    payload = notePayload transcript pcm
    operation = normalizeReceipt <$> prepare
    prepare = do
      encoded <- encode pcm
      continue (deliver owner rules gmailRules send clock ident transcript) encoded

deliver :: IO (Result Text) -> NoteMailRules -> GmailRules -> Send -> IO UTCTime -> Text -> Text -> ByteString -> IO (Result Value)
deliver owner rules gmailRules send clock ident transcript audio = do
  sender <- owner
  now <- clock
  continue (submit now) sender
  where
    submit now origin = do
      let (envelope, file) = noteMessage rules now origin ident transcript audio
      result <- sendEnvelope gmailRules send envelope [file] Nothing
      pure (toJSON (Receipt "ok" "sent") <$ result)
