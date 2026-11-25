{-# LANGUAGE TupleSections #-}
module EventsSpec (spec) where

import Baerbel.Runtime.Events
import Baerbel.Core.Error (failure)
import Baerbel.Runtime.Logging (Report)
import Baerbel.Runtime.Messages (Messages(..))
import Baerbel.Core.SMS (Receipt(..), Delivery(..))
import Baerbel.Runtime.Storage (withDatabase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import Data.IORef
import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute_)
import System.Directory (renameFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import TestSupport (withRule)

spec :: Spec
spec = describe "durable PBX event replay" $ do
  it "leaves an incomplete final record for the next poll" $ withReader $ \rules source connection -> do
    seen <- newIORef []
    let messages = recorder seen
    Bytes.writeFile source (record "42.1" <> "incoming-v1,42.2,1730000000")
    pollEvents rules quiet messages source connection `shouldReturn` Right 1
    Bytes.appendFile source ",allowed,+447700900123,SCREENING,0\n"
    pollEvents rules quiet messages source connection `shouldReturn` Right 1
    readIORef seen `shouldReturn` ["42.1", "42.2"]
  it "follows inode identity through rename/create rotation" $ withReader $ \rules source connection -> do
    seen <- newIORef []
    let messages = recorder seen
    Bytes.writeFile source (record "42.1")
    pollEvents rules quiet messages source connection `shouldReturn` Right 1
    renameFile source (source <> ".1")
    Bytes.writeFile source (record "42.2")
    pollEvents rules quiet messages source connection `shouldReturn` Right 1
    readIORef seen `shouldReturn` ["42.1", "42.2"]
  it "detects rewritten contents even when the same inode is still long enough" $ withReader $ \rules source connection -> do
    seen <- newIORef []
    let messages = recorder seen
    Bytes.writeFile source (record "42.1")
    pollEvents rules quiet messages source connection `shouldReturn` Right 1
    Bytes.writeFile source (record "42.2")
    pollEvents rules quiet messages source connection `shouldReturn` Right 1
    readIORef seen `shouldReturn` ["42.1", "42.2"]
  it "checkpoints malformed complete records and emits only a fixed diagnostic" $ withReader $ \rules source connection -> do
    seen <- newIORef []
    events <- newIORef []
    let messages = recorder seen
        report name fields = modifyIORef' events (<> [(name, fields)])
    Bytes.writeFile source ("bad,private,record\n" <> record "42.1")
    pollEvents rules report messages source connection `shouldReturn` Right 1
    pollEvents rules report messages source connection `shouldReturn` Right 0
    readIORef events `shouldReturn` [("incoming_event_invalid", [])]
    readIORef seen `shouldReturn` ["42.1"]
  it "does not checkpoint an event when durable reservation requests retry" $ withReader $ \rules source connection -> do
    seen <- newIORef []
    first <- newIORef True
    let messages = (recorder seen) {deliverOutcome = retryOnce first seen}
    Bytes.writeFile source (record "42.1" <> record "42.2")
    pollEvents rules quiet messages source connection `shouldReturn` Right 0
    pollEvents rules quiet messages source connection `shouldReturn` Right 2
    readIORef seen `shouldReturn` ["42.1", "42.1", "42.2"]
  it "rejects an oversized unterminated record without advancing the cursor" $ withReader $ \rules source connection -> do
    seen <- newIORef []
    let size = eventMaximumLine rules + 1
        bytes = Bytes.replicate size 65
        messages = recorder seen
    Bytes.writeFile source bytes
    pollEvents rules quiet messages source connection `shouldReturn` Left (failure "incoming_event_record_too_large")
    Bytes.writeFile source (record "42.1")
    pollEvents rules quiet messages source connection `shouldReturn` Right 1

withReader :: (EventRules -> FilePath -> Connection -> Expectation) -> Expectation
withReader check = withRule "incoming_events" "reader" $ \rules ->
  withSystemTempDirectory "baerbel-events" (run rules)
  where
  run rules directory = do
    let source = directory </> "events.csv"
        database = directory </> "cursor.sqlite3"
    outcome <- withDatabase "test_database" database (initialized rules source)
    outcome `shouldBe` Right ()
  initialized rules source connection = do
    execute_ connection "CREATE TABLE incoming_event_cursors (file_key TEXT PRIMARY KEY, position INTEGER NOT NULL, anchor_length INTEGER NOT NULL, anchor TEXT NOT NULL)"
    check rules source connection
    pure (Right ())

record :: ByteString -> ByteString
record ident = "incoming-v1," <> ident <> ",1730000000,allowed,+447700900123,SCREENING,0\n"

quiet :: Report
quiet _ _ = pure ()

recorder :: IORef [Text] -> Messages
recorder seen = Messages (const (pure Nothing)) (const (pure ()))
  (\_ _ -> pure ()) (\_ _ -> pure ()) (\_ _ -> pure ())
  (\_ _ _ _ _ -> pure accepted) outcome
  where outcome ident _ _ _ = modifyIORef' seen (<> [ident]) >> pure accepted

accepted :: Receipt
accepted = Receipt Sent Nothing Nothing

retryOnce :: IORef Bool -> IORef [Text] -> Text -> Text -> Maybe Text -> Bool -> IO Receipt
retryOnce first seen ident _ _ _ = do
  retry <- atomicModifyIORef' first (False,)
  modifyIORef' seen (<> [ident])
  let receipt | retry = Receipt Failed Nothing (Just True)
              | otherwise = accepted
  pure receipt
