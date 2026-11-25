module BoundarySpec (spec) where

import Baerbel.Runtime.Config (Settings, loadSettings, rule)
import Baerbel.Core.Error (Result)
import Baerbel.Core.Events (Event(..), EventRules, parseEvent)
import Baerbel.Core.HTTP (Response(..))
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Runtime.Ledger (withLedger, durable, lookupExisting)
import Baerbel.Runtime.SMS
import Baerbel.Core.Speech (splitFrames)
import Baerbel.Runtime.VAD (withVAD, isSpeech, resetVAD)
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Async (async, cancel)
import Data.Aeson (FromJSON(..), Value, object, (.=), genericParseJSON, eitherDecodeFileStrict')
import qualified Data.ByteString.Base64 as Base64
import qualified Data.IORef
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

data SMSCase = SMSCase {smsCaseInput :: Text, smsCaseOutput :: Maybe Text} deriving (Generic)
instance FromJSON SMSCase where parseJSON = genericParseJSON (jsonOptions 7)
data EventCase = EventCase {eventCaseRaw :: Text, eventCaseExpected :: Maybe Value} deriving (Generic)
instance FromJSON EventCase where parseJSON = genericParseJSON (jsonOptions 9)
data VADCase = VADCase
  { vadCaseRate :: Int, vadCaseFrameBytes :: Int, vadCaseAudio :: Text, vadCaseResults :: [[Bool]] } deriving (Generic)
instance FromJSON VADCase where parseJSON = genericParseJSON (jsonOptions 7)

withSettings :: (Settings -> IO ()) -> IO ()
withSettings action = loadSettings "config/global.yaml" "config/user.example.json" >>= expectResult action

expectResult :: (a -> IO ()) -> Result a -> IO ()
expectResult = either (expectationFailure . show)

fixtures :: FromJSON a => FilePath -> ([a] -> IO ()) -> IO ()
fixtures path action = eitherDecodeFileStrict' path >>= either expectationFailure action

spec :: Spec
spec = do
  describe "SMS and PBX Python compatibility" $ do
    it "matches UTF-16 truncation and external text rejection" $ withSettings $ \settings ->
      expectResult (fixtures "test/fixtures/sms.json" . mapM_ . verifySMS) (rule settings "sms" "transport")
    it "matches CSV validation and private caller suppression" $ withSettings $ \settings ->
      expectResult (fixtures "test/fixtures/events.json" . mapM_ . verifyEvent) (rule settings "incoming_events" "reader")
    it "does not resend an uncertain message as an outcome" $ withSMSFixture Uncertain $ \sender counter -> do
      first <- sender "incoming:42.1" "original message" Message
      second <- sender "incoming:42.1" "call outcome" Outcome
      receiptStatus first `shouldBe` Uncertain
      receiptStatus second `shouldBe` Uncertain
      receiptDuplicate second `shouldBe` Just True
      readIORef counter `shouldReturn` 1
    it "allows exactly one outcome after a definitively rejected message" $ withSMSFixture Failed $ \sender counter -> do
      _ <- sender "incoming:42.2" "original message" Message
      _ <- sender "incoming:42.2" "call outcome" Outcome
      duplicate <- sender "incoming:42.2" "call outcome" Outcome
      receiptDuplicate duplicate `shouldBe` Just True
      readIORef counter `shouldReturn` 2
  describe "durable mutation boundaries" $ do
    it "preserves Unicode receipts across restart" unicodeReceipt
    it "keeps an interrupted reservation and never starts a second send" interruptedReservation
  describe "native VAD compatibility" $ do
    it "matches all four Python modes across supported sample rates and continuous history" $
      fixtures "test/fixtures/vad.json" (mapM_ verifyVAD)

verifySMS :: SMSRules -> SMSCase -> IO ()
verifySMS rules item = either (const Nothing) Just (boundedSMS rules (smsCaseInput item)) `shouldBe` smsCaseOutput item

verifyEvent :: EventRules -> EventCase -> IO ()
verifyEvent rules item = case Base64.decode (Text.encodeUtf8 (eventCaseRaw item)) of
  Left reason -> expectationFailure reason
  Right raw -> fmap projection (parseEvent rules raw) `shouldBe` eventCaseExpected item
  where
  projection value = object ["event_id" .= eventIdentifier value, "reason" .= eventReason value,
    "callback_number" .= eventCallback value, "answered" .= eventAnswered value]

withSMSFixture :: Delivery -> (SMS -> Data.IORef.IORef Int -> IO ()) -> IO ()
withSMSFixture status action = withSettings $ \settings -> expectResult prepare (rule settings "sms" "transport")
  where
  credentials = SMSCredentials "+441234567890" "synthetic" "+447700900123"
  prepare rules = withSystemTempDirectory "baerbel-sms" $ \directory -> do
    count <- newIORef 0
    let path = directory </> "sms.sqlite3"
        response | status == Failed = Response 200 [] "ERR: rejected"
                 | otherwise = Response 502 [] ""
        sender _ = modifyIORef' count (+ 1) >> pure (Right response)
    result <- withSMS rules credentials sender path (\sms -> action sms count >> pure (Right ()))
    result `shouldBe` Right ()

unicodeReceipt :: IO ()
unicodeReceipt = withSystemTempDirectory "baerbel-ledger" $ \directory -> do
  let path = directory </> "sends.sqlite3"
      arguments = object ["subject" .= ("Bärbel 😀" :: Text)]
      receipt = object ["status" .= ("ok" :: Text), "text" .= ("Grüße 😀" :: Text)]
      duplicate = object ["status" .= ("ok" :: Text), "text" .= ("Grüße 😀" :: Text), "duplicate" .= True]
  saved <- withLedger path (\ledger -> durable ledger "test-1" "gmail_send" arguments (pure (Right receipt)))
  saved `shouldBe` Right receipt
  loaded <- withLedger path (\ledger -> lookupExisting ledger "test-1" "gmail_send" arguments)
  loaded `shouldBe` Right (Just duplicate)

interruptedReservation :: IO ()
interruptedReservation = withSystemTempDirectory "baerbel-interrupted" $ \directory -> do
  let path = directory </> "sends.sqlite3"
      arguments = object []
  started <- newEmptyMVar
  blocked <- newEmptyMVar
  count <- newIORef (0 :: Int)
  let operation = modifyIORef' count (+ 1) >> putMVar started () >> takeMVar blocked
  task <- async (withLedger path (\ledger -> durable ledger "test-2" "gmail_send" arguments operation))
  takeMVar started
  cancel task
  replay <- withLedger path (\ledger -> durable ledger "test-2" "gmail_send" arguments operation)
  let expected = object ["status" .= ("unknown" :: Text), "error" .= ("send_in_progress_or_interrupted" :: Text), "retry_safe" .= False, "duplicate" .= True]
  replay `shouldBe` Right expected
  readIORef count `shouldReturn` 1

verifyVAD :: VADCase -> IO ()
verifyVAD item = case Base64.decode (Text.encodeUtf8 (vadCaseAudio item)) of
  Left reason -> expectationFailure reason
  Right audio -> mapM_ (mode frames) (zip [0..] (vadCaseResults item))
    where (frames, _) = splitFrames (vadCaseFrameBytes item) audio
  where
  mode frames (number, expected) = do
    result <- withVAD number $ \vad -> do
      first <- sequence <$> traverse (isSpeech vad (vadCaseRate item)) frames
      first `shouldBe` Right expected
      reset <- resetVAD vad number
      reset `shouldBe` Right ()
      sequence <$> traverse (isSpeech vad (vadCaseRate item)) frames
    result `shouldBe` Right expected
