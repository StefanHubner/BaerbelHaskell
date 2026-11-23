module MessagesSpec (spec) where

import Baerbel.Config (loadSettings, rule)
import Baerbel.Incoming (Decision(..))
import Baerbel.Messages (Messages(..), MessageRules(..), Context, withMessages)
import Baerbel.SMS (Receipt(..), Delivery(Sent, Failed), Kind(Message, Outcome))
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync, wait, cancel)
import Control.Monad (when)
import System.Timeout (timeout)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec hiding (context)

spec :: Spec
spec = describe "Incoming context retention" $ do
  it "evicts the oldest inactive insertion, independent of key order or reactivation" $ withFixture $ \rules -> do
    sent <- newMVar []
    let sender key body _ = modifyMVar_ sent (pure . ((key, body):)) >> pure (Receipt Sent Nothing Nothing)
    result <- withMessages rules (\_ _ -> pure ()) sender $ \service -> do
      remembered service "z-first" "Zed"
      remembered service "a-second" "Ada"
      reactivate service "z-first"
      remembered service "m-third" "Mia"
      _ <- deliverOutcome service "a-second" "NOANSWER" Nothing False
      _ <- deliverOutcome service "z-first" "NOANSWER" Nothing False
      pure (Right ())
    result `shouldBe` Right ()
    messages <- readMVar sent
    lookup "incoming:a-second" messages `shouldSatisfy` maybe False ("Ada" `T.isInfixOf`)
    lookup "incoming:z-first" messages `shouldSatisfy` maybe False (not . ("Zed" `T.isInfixOf`))
  it "marks known forwarding and preserves facts when a later decision has empty fields" $ withFixture $ \rules -> do
    sent <- newMVar []
    let sender _ body _ = modifyMVar_ sent (pure . (body:)) >> pure (Receipt Sent Nothing Nothing)
    result <- withMessages rules (\_ _ -> pure ()) sender $ \service -> do
      context <- beginContext service "known"
      case context of
        Nothing -> expectationFailure "missing context"
        Just value -> knownContext service value "Ada" >> finishContext service value
      _ <- deliverOutcome service "known" "BUSY" Nothing False
      preserved <- beginContext service "preserved"
      case preserved of
        Nothing -> expectationFailure "missing context"
        Just value -> do
          knownContext service value "Bob"
          decideContext service value (Decline "caller_declined" "" "")
          finishContext service value
      _ <- deliverOutcome service "preserved" "DECLINED" Nothing False
      pure (Right ())
    result `shouldBe` Right ()
    messages <- readMVar sent
    messages `shouldSatisfy` any ("I tried to put them through" `T.isInfixOf`)
    messages `shouldSatisfy` any ("Bob called." `T.isInfixOf`)
  it "retains bounded message facts for a later missed-call outcome" $ withFixture $ \rules -> do
    sent <- newMVar ""
    let sender _ rendered _ = modifyMVar_ sent (const (pure rendered)) >> pure (Receipt Sent Nothing Nothing)
        name = T.replicate 50 "n"
        body = T.replicate 200 "x" <> "\nextra"
        retainedName = T.take (messageNameChars rules) name
        retainedSummary = T.take (messageSummaryChars rules) body
    result <- withMessages rules (\_ _ -> pure ()) sender $ \service -> do
      context <- beginContext service "message"
      case context of
        Nothing -> expectationFailure "missing context"
        Just value -> decideContext service value (LeaveMessage name body) >> finishContext service value
      _ <- deliverOutcome service "message" "MESSAGE_FAILED" Nothing False
      pure (Right ())
    result `shouldBe` Right ()
    rendered <- readMVar sent
    rendered `shouldSatisfy` T.isPrefixOf (retainedName <> " called.")
    rendered `shouldSatisfy` T.isInfixOf (retainedSummary <> ".")
    rendered `shouldSatisfy` (not . T.isInfixOf name)
  it "does not report an already-started outcome as delivery of a new caller message" $ withFixture $ \rules -> do
    started <- newEmptyMVar
    release <- newEmptyMVar
    let sender _ _ kind = do
          when (kind == Outcome) (putMVar started () >> takeMVar release)
          pure (Receipt Sent Nothing Nothing)
    result <- withMessages rules (\_ _ -> pure ()) sender $ \service -> withAsync (deliverOutcome service "started" "BUSY" Nothing False) $ \task -> do
      takeMVar started
      message <- timeout 1000000 (deliverMessage service "started" "call" "Name" "Message" Nothing)
      putMVar release ()
      completed <- wait task
      fmap receiptStatus message `shouldBe` Just Failed
      receiptStatus completed `shouldBe` Sent
      pure (Right ())
    result `shouldBe` Right ()
  it "finishes and reports an accepted delivery after the caller task is cancelled" $ withFixture $ \rules -> do
    started <- newEmptyMVar
    release <- newEmptyMVar
    reported <- newMVar []
    let sender _ _ _ = putMVar started () >> takeMVar release >> pure (Receipt Sent Nothing Nothing)
        report event fields = modifyMVar_ reported (pure . ((event, fields):))
    result <- withMessages rules report sender $ \service -> do
      withAsync (deliverMessage service "cancelled" "call" "Ada" "Hello" Nothing) $ \task -> do
        takeMVar started
        cancel task
      putMVar release ()
      pure (Right ())
    result `shouldBe` Right ()
    events <- readMVar reported
    map fst events `shouldBe` ["incoming_message_delivery"]
  it "sends the missed-call outcome when the replacement caller message fails" $ withFixture $ \rules -> do
    sent <- newMVar []
    let sender _ _ kind = modifyMVar_ sent (pure . (kind:)) >> pure (Receipt (if kind == Outcome then Sent else Failed) Nothing Nothing)
    result <- withMessages rules (\_ _ -> pure ()) sender $ \service -> do
      context <- beginContext service "replacement"
      withAsync (deliverOutcome service "replacement" "MESSAGE_FAILED" Nothing False) $ \task -> do
        delivered <- deliverMessage service "replacement" "call" "Ada" "Hello" Nothing
        receiptStatus delivered `shouldBe` Failed
        maybe (pure ()) (finishContext service) context
        final <- wait task
        receiptStatus final `shouldBe` Sent
      pure (Right ())
    result `shouldBe` Right ()
    reverse <$> readMVar sent `shouldReturn` [Message, Outcome]
  it "retains active contexts and refuses capacity when all entries are active" $ withFixture $ \rules -> do
    let sender _ _ _ = pure (Receipt Sent Nothing Nothing)
    result <- withMessages rules (\_ _ -> pure ()) sender $ \service -> do
      first <- beginContext service "first"
      second <- beginContext service "second"
      third <- beginContext service "third"
      isNothing third `shouldBe` True
      mapM_ (maybe (pure ()) (finishContext service)) [first, second]
      pure (Right ())
    result `shouldBe` Right ()

withFixture :: (MessageRules -> Expectation) -> Expectation
withFixture action = do
  configured <- loadSettings "config/global.json" "config/user.example.json"
  case configured >>= \settings -> rule settings "incoming_messages" "service" of
    Left problem -> expectationFailure (show problem)
    Right rules -> action rules {messageContextLimit = 2}

remembered :: Messages -> Text -> Text -> IO ()
remembered service identifier name = do
  context <- beginContext service identifier
  maybe (expectationFailure "context not allocated") remember context
  where
    remember :: Context -> IO ()
    remember context = knownContext service context name >> finishContext service context

reactivate :: Messages -> Text -> IO ()
reactivate service identifier = beginContext service identifier >>= maybe (expectationFailure "existing context lost") (finishContext service)
