module CoreSpec (spec) where

import Baerbel.Core.Accounts
import Baerbel.Core.Pin (encodePinHash, parsePinHash, verifyPin)
import Baerbel.Runtime.Config
import Baerbel.Core.Error (Failure, failure)
import Baerbel.Core.OAuth (Credentials(..))
import Baerbel.Core.Profile (ProfileState(..), emptyProfile)
import Baerbel.Core.Store (StoreRules(..))
import Baerbel.Runtime.Ledger (durable, withAccountLedger)
import Baerbel.Runtime.Profile (ProfileStore(..))
import Baerbel.Runtime.Storage (AccountStore(..))
import Baerbel.Runtime.UserDatabase (accountMemory, googleCredentials, saveGoogleCredentials, readAccounts)
import Control.Monad (forM_, void)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.SQLite.Simple (Query(..), execute_, withConnection)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Baerbel.Core.JSON (camelOptions, decodeBytes)
import Baerbel.Core.Ledger (fingerprint)
import Data.Aeson (FromJSON (..), Value(..), eitherDecodeFileStrict', genericParseJSON)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Hspec

data FingerprintCase = FingerprintCase
  { caseName :: Text, caseArguments :: Value, caseFingerprint :: Text }
  deriving (Show, Generic)
instance FromJSON FingerprintCase where
  parseJSON = genericParseJSON (camelOptions 4)

spec :: Spec
spec = do
  describe "typed boundary" $ do
    it "returns malformed JSON as an error value" $
      (decodeBytes "invalid" "{" :: Either Failure Text) `shouldBe` Left (failure "invalid")
    it "loads the preserved global tool catalogue" $ do
      result <- loadSettings "config/global.yaml" "config/user.example.json"
      fmap (Map.size . globalTools . settingsGlobal) result `shouldBe` Right 15
  describe "account separation" $ do
    it "selects a phone account with private state and central model policy" $ do
      let selected = accountSettings accountGlobal Map.empty accountConfig
          result = fmap (fmap accountValues . (`accountForPhone` "07700900123")) selected
      result `shouldBe` Right (Just ("pilot", "false", "shared-model", "Hello Pilot"))
    it "keeps Google grants, memory and receipt IDs bound to their user" $ withUserFixture $ \standard pro -> do
      let credential = Credentials "client" "secret" "refresh" (Set.singleton "scope") Nothing "authorized_user"
          rules = StoreRules 4096 100000 1000
          current = ProfileState 1 (Map.singleton "home" "Bristol") (Just "2026-11-25T20:00:00Z")
          other = accountMemory rules standard
          owner = accountMemory rules pro
      saveGoogleCredentials pro "gmail" credential `shouldReturn` Right ()
      fmap (fmap (fmap refreshToken)) (googleCredentials standard "gmail") `shouldReturn` Right Nothing
      fmap (fmap (fmap refreshToken)) (googleCredentials pro "gmail") `shouldReturn` Right (Just "refresh")
      profileUpdate owner (const (Right (current, Null))) `shouldReturn` Right Null
      profileRead owner `shouldReturn` Right current
      profileRead other `shouldReturn` Right emptyProfile
      receipt pro "gmail" "owner" `shouldReturn` Right (String "owner")
      receipt pro "gmail" "duplicate" `shouldReturn` Right (String "owner")
      receipt pro "calendar" "calendar" `shouldReturn` Right (String "calendar")
      receipt standard "gmail" "standard" `shouldReturn` Right (String "standard")
      fmap (fmap Map.size) (readAccounts (accountStorePath pro)) `shouldReturn` Right 2
    it "rejects model and carrier settings in the wrong scope" $ do
      let shared = Map.singleton "AA_SIP_HOST" "pbx"
      fmap Map.size (accountSettings accountGlobal shared accountConfig) `shouldBe` Left (failure "unsupported_account_settings:shared:AA_SIP_HOST")
      scopedResult Standard "OPENAI_CHAT_MODEL" `shouldBe` Left (failure "unsupported_account_settings:pilot:OPENAI_CHAT_MODEL")
    it "rejects settings fixed by the account plan" $ do
      scopedResult Standard "INCOMING_SMS_ENABLED" `shouldBe` Left (failure "unsupported_account_settings:pilot:INCOMING_SMS_ENABLED")
      scopedResult Pro "PHONE_ASSISTANT_DIRECT_CALLS" `shouldBe` Left (failure "unsupported_account_settings:pilot:PHONE_ASSISTANT_DIRECT_CALLS")
      scopedResult Standard "PHONE_ASSISTANT_DIRECT_CALLS" `shouldBe` Right 1
    it "rejects individual accounts in the global deployment section" $ do
      let bad = "{\"defaults\":{},\"rules\":{},\"tools\":{},\"installation\":{\"environment\":{},\"accounts\":{},\"database\":\"/private/users.sqlite3\"}}"
          decoded = decodeBytes "bad" bad :: Either Failure Global
      void decoded `shouldBe` Left (failure "bad")
    it "rejects shared caller identities across accounts" $ do
      let duplicated = Map.fromList [("pilot", pilot), ("other", pilot)]
      fmap Map.size (accountSettings accountGlobal Map.empty duplicated) `shouldBe` Left (failure "invalid_accounts")
    it "verifies the generated PIN hash with the existing verifier" $ do
      let encoded = encodePinHash "0123456789abcdef" "7391"
      fmap (`verifyPin` "7391") (parsePinHash encoded) `shouldBe` Right True
      fmap (`verifyPin` "1234") (parsePinHash encoded) `shouldBe` Right False
  describe "durable compatibility" $ do
    it "matches Python's persisted request fingerprints" $ do
      loaded <- eitherDecodeFileStrict' "test/fixtures/ledger.json" :: IO (Either String [FingerprintCase])
      case loaded of
        Left reason -> expectationFailure reason
        Right cases -> mapM_ verify cases
  where
  verify item = fingerprint (caseName item) (caseArguments item) `shouldBe` caseFingerprint item

accountGlobal :: Global
accountGlobal = Global (Map.singleton "OPENAI_CHAT_MODEL" "shared-model") Map.empty Map.empty Nothing

accountConfig :: Map.Map Text Account
accountConfig = Map.singleton "pilot" pilot

pilot :: Account
pilot = Account Standard "Pilot" "+447700900123" values
  where
    values = Map.fromList
      [("PHONE_ASSISTANT_PRIVATE_PIN_HASH", pin), ("PHONE_ASSISTANT_GREETING", "Hello Pilot")]
    pin = "pbkdf2_sha256:600000:00000000000000000000000000000000:0000000000000000000000000000000000000000000000000000000000000000"

withUserFixture :: (AccountStore -> AccountStore -> IO ()) -> IO ()
withUserFixture action = withSystemTempDirectory "baerbel-user-test" prepare
  where
    prepare directory = do
      let path = directory </> "users.sqlite3"
      schema <- Text.readFile "config/users.sql"
      withConnection path (\connection -> do
        let statements = filter (not . Text.null) (map Text.strip (Text.splitOn ";" schema))
        forM_ statements (execute_ connection . Query)
        execute_ connection "INSERT INTO users(id,user_type,name,phone,pin_hash) VALUES ('pilot','Standard','Pilot','+447700900123','hash')"
        execute_ connection "INSERT INTO users(id,user_type,name,phone,pin_hash) VALUES ('pro','Pro','Owner','+447700900124','hash')")
      setFileMode path 0o600
      action (AccountStore path "pilot") (AccountStore path "pro")

receipt :: AccountStore -> Text -> Text -> IO (Either Failure Value)
receipt store service text = withAccountLedger store service prepare
  where prepare ledger = durable ledger "same-id" "send" Null (pure (Right (String text)))

scopedResult :: Plan -> Text -> Either Failure Int
scopedResult plan key = fmap Map.size (accountSettings accountGlobal Map.empty config)
  where
    values = Map.insert key "true" (accountEnvironment pilot)
    sub = pilot {accountPlan = plan, accountEnvironment = values}
    config = Map.singleton "pilot" sub

accountValues :: (Text, Subscriber) -> (Text, Text, Text, Text)
accountValues (ident, sub) = (ident, value "INCOMING_SCREENING_ENABLED", value "OPENAI_CHAT_MODEL", value "PHONE_ASSISTANT_GREETING")
  where value = setting (subscriberSettings sub)
