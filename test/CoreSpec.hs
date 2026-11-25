module CoreSpec (spec) where

import Baerbel.Core.Accounts
import Baerbel.Core.Pin (encodePinHash, parsePinHash, verifyPin)
import Baerbel.Runtime.Config
import Baerbel.Core.Error (Failure, failure)
import Baerbel.Core.JSON (camelOptions, decodeBytes)
import Baerbel.Core.Ledger (fingerprint)
import Data.Aeson (FromJSON (..), Value, eitherDecodeFileStrict', genericParseJSON)
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
      result <- loadSettings "config/global.json" "config/user.example.json"
      fmap (Map.size . globalTools . settingsGlobal) result `shouldBe` Right 15
  describe "account separation" $ do
    it "selects a phone account with private state and central model policy" $ do
      let selected = accountSettings accountGlobal accountConfig
          result = fmap (fmap accountValues . (`accountForPhone` "07700900123")) selected
      result `shouldBe` Right (Just ("pilot", "false", "shared-model", "/private/pilot.json"))
    it "rejects shared caller identities across accounts" $ do
      let duplicated = accountConfig {installationAccounts = Just (Map.fromList [("pilot", pilot), ("other", pilot)])}
      fmap Map.size (accountSettings accountGlobal duplicated) `shouldBe` Left (failure "invalid_accounts")
    it "verifies the generated PIN hash with the existing verifier" $ do
      let encoded = encodePinHash "0123456789abcdef" "73910426"
      fmap (`verifyPin` "73910426") (parsePinHash encoded) `shouldBe` Right True
      fmap (`verifyPin` "12345678") (parsePinHash encoded) `shouldBe` Right False
  describe "durable compatibility" $ do
    it "matches Python's persisted request fingerprints" $ do
      loaded <- eitherDecodeFileStrict' "test/fixtures/ledger.json" :: IO (Either String [FingerprintCase])
      case loaded of
        Left reason -> expectationFailure reason
        Right cases -> mapM_ verify cases
  where
  verify item = fingerprint (caseName item) (caseArguments item) `shouldBe` caseFingerprint item

accountGlobal :: Global
accountGlobal = Global (Map.singleton "OPENAI_CHAT_MODEL" "shared-model") Map.empty Map.empty

accountConfig :: Installation
accountConfig = Installation Map.empty (Just (Map.singleton "pilot" pilot))

pilot :: Account
pilot = Account Standard "Pilot" "+447700900123" values
  where
    values = Map.fromList
      [("PHONE_ASSISTANT_PRIVATE_PIN_HASH", pin), ("INCOMING_SCREENING_ENABLED", "true")
      , ("OPENAI_CHAT_MODEL", "private-model"), ("PHONE_ASSISTANT_PROFILE_FILE", "/private/pilot.json")]
    pin = "pbkdf2_sha256:600000:00000000000000000000000000000000:0000000000000000000000000000000000000000000000000000000000000000"

accountValues :: (Text, Subscriber) -> (Text, Text, Text, Text)
accountValues (ident, sub) = (ident, value "INCOMING_SCREENING_ENABLED", value "OPENAI_CHAT_MODEL", value "PHONE_ASSISTANT_PROFILE_FILE")
  where value = setting (subscriberSettings sub)
