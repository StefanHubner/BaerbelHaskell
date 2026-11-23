module NoteMailSpec (spec) where

import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Request(..), Send)
import Baerbel.Ledger (durable, withLedger)
import Baerbel.NoteMail
import Baerbel.ProviderConfig (GmailRules)
import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import Data.IORef
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import TestSupport (withRule, withRules)

spec :: Spec
spec = describe "fixed-owner note mail delivery" $ do
  it "stores definite encoding failures in the original note receipt shape" $ withConfig $ \rules gmailRules ->
    withSystemTempDirectory "baerbel-note" $ \directory -> do
      let path = directory </> "sends.sqlite3"
          encode _ = pure (Left (failure "note_encoding_failed"))
          sender _ = expectationFailure "HTTP reached after encoding failure" >> pure (Left (failure "unexpected"))
      result <- withLedger path $ \ledger -> do
        deliver <- noteSessionWith encode rules gmailRules sender (durable ledger) clock
        deliver "note" "words" "pcm"
      result `shouldBe` Right (rejected "error" "note_send_failed")
      duplicate <- withLedger path $ \ledger -> do
        deliver <- noteSessionWith encode rules gmailRules sender (durable ledger) clock
        deliver "note" "words" "pcm"
      duplicate `shouldBe` Right (object ["status" .= ("error" :: Text), "error" .= ("note_send_failed" :: Text), "duplicate" .= True])
  it "maps an accepted mutation with unknown outcome to the original note failure" $ withConfig $ \rules gmailRules -> do
    let sender = reply (Left (Failure "gmail_connection_failed" True))
    deliver <- noteSessionWith encoded rules gmailRules sender direct clock
    deliver "note" "words" "pcm" `shouldReturn` Right (rejected "unknown" "note_send_outcome_unknown_do_not_retry")
  it "maps account lookup failure without claiming a send was attempted" $ withConfig $ \rules gmailRules -> do
    calls <- newIORef []
    let sender request = modifyIORef' calls (<> [requestMethod request]) >> pure (Left (failure "gmail_refresh_scopes_rejected"))
    deliver <- noteSessionWith encoded rules gmailRules sender direct clock
    deliver "note" "words" "pcm" `shouldReturn` Right (rejected "error" "note_send_failed")
    readIORef calls `shouldReturn` ["GET"]
  it "shares a successful owner address within one call while sending each accepted note once" $ withConfig $ \rules gmailRules -> do
    calls <- newIORef []
    let sender request = modifyIORef' calls (<> [requestMethod request]) >> reply sent request
    deliver <- noteSessionWith encoded rules gmailRules sender direct clock
    deliver "first" "one" "pcm" `shouldReturn` Right completed
    deliver "second" "two" "pcm" `shouldReturn` Right completed
    readIORef calls `shouldReturn` ["GET", "POST", "POST"]
  it "does not report success after receipt persistence is uncertain" $ withConfig $ \rules gmailRules -> do
    let persist _ _ _ operation = operation >> pure (Left (Failure "send_receipt_storage_failed" True))
    deliver <- noteSessionWith encoded rules gmailRules (reply sent) persist clock
    deliver "note" "words" "pcm" `shouldReturn` Right (rejected "unknown" "note_send_outcome_unknown_do_not_retry")

withConfig :: (NoteMailRules -> GmailRules -> Expectation) -> Expectation
withConfig check = withRule "notes" "mail" (withRules "gmail" . check)

clock :: IO UTCTime
clock = pure (UTCTime (fromGregorian 2026 11 19) 0)

encoded :: ByteString -> IO (Result ByteString)
encoded _ = pure (Right "encoded-mp3")

direct :: Text -> Text -> Value -> IO (Result Value) -> IO (Result Value)
direct _ _ _ operation = operation

reply :: Result Value -> Send
reply result request
  | requestMethod request == "GET" = pure (Right (object ["emailAddress" .= ("owner@example.com" :: Text)]))
  | otherwise = pure result

sent :: Result Value
sent = Right (object ["id" .= ("a1" :: Text), "threadId" .= ("b2" :: Text)])

completed :: Value
completed = object ["status" .= ("ok" :: Text), "result" .= ("sent" :: Text)]

rejected :: Text -> Text -> Value
rejected status code = object ["status" .= status, "error" .= code]
