module ContactsSpec (spec) where

import Baerbel.Runtime.Contacts (contacts)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Runtime.HTTP (Request(..), Send)
import Baerbel.Core.ProviderConfig (ContactsRules(..))
import Data.Aeson hiding (Result)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec
import TestSupport (withRules)

emptyMatches :: Value
emptyMatches = object ["results" .= ([] :: [Value])]

arguments :: Text -> Int -> Value
arguments query limit = object ["query" .= query, "limit" .= limit]

recordSend :: IORef [Request] -> IORef [Result Value] -> Send
recordSend requests responses request = do
  modifyIORef' requests (<> [request])
  atomicModifyIORef' responses takeResponse
  where
    takeResponse :: [Result Value] -> ([Result Value], Result Value)
    takeResponse [] = ([], Left (failure "unexpected_request"))
    takeResponse (result : rest) = (rest, result)

requested :: ByteString -> [Request] -> [Maybe (Maybe ByteString)]
requested key = map (lookup key . requestQuery)

invalidQueries :: ContactsRules -> Expectation
invalidQueries rules = do
  requests <- newIORef []
  responses <- newIORef []
  execute <- contacts rules (recordSend requests responses)
  let invalid = [arguments "  " 1, arguments "Alice" 0, arguments "Alice" (contactsSearchLimit rules + 1), object []]
  mapM_ (\value -> execute "contacts_search" value "id" `shouldReturn` Left (failure "contacts_invalid_arguments")) invalid
  readIORef requests `shouldReturn` []

successfulWarmup :: ContactsRules -> Expectation
successfulWarmup rules = do
  requests <- newIORef []
  responses <- newIORef (replicate 3 (Right emptyMatches))
  let immediate = rules {contactsWarmupSeconds = 0}
  execute <- contacts immediate (recordSend requests responses)
  first <- execute "contacts_search" (arguments " Alice " 1) "first"
  second <- execute "contacts_search" (arguments "Bob" 2) "second"
  first `shouldBe` second
  first `shouldSatisfy` either (const False) (const True)
  sent <- readIORef requests
  requested "query" sent `shouldBe` map (Just . Just) ["", "Alice", "Bob"]
  requested "pageSize" sent `shouldBe` map (Just . Just) ["1", "2", "3"]

failedWarmup :: ContactsRules -> Expectation
failedWarmup rules = do
  requests <- newIORef []
  responses <- newIORef [Left (failure "contacts_unavailable"), Right emptyMatches, Right emptyMatches]
  let immediate = rules {contactsWarmupSeconds = 0}
  execute <- contacts immediate (recordSend requests responses)
  execute "contacts_search" (arguments "Alice" 1) "first" `shouldReturn` Left (failure "contacts_unavailable")
  result <- execute "contacts_search" (arguments "Alice" 1) "second"
  result `shouldSatisfy` either (const False) (const True)
  sent <- readIORef requests
  requested "query" sent `shouldBe` map (Just . Just) ["", "", "Alice"]

spec :: Spec
spec = describe "Contacts request boundary" $ do
  it "rejects invalid queries before warming or searching" (withRules "contacts" invalidQueries)
  it "normalizes queries and reuses only a successful warmup" (withRules "contacts" successfulWarmup)
  it "preserves a warmup failure and allows the next explicit request" (withRules "contacts" failedWarmup)
