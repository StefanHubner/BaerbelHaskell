module CoreSpec (spec) where

import Baerbel.Config
import Baerbel.Error (Failure, failure)
import Baerbel.JSON (camelOptions, decodeBytes)
import Baerbel.Ledger (fingerprint)
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
  describe "durable compatibility" $ do
    it "matches Python's persisted request fingerprints" $ do
      loaded <- eitherDecodeFileStrict' "test/fixtures/ledger.json" :: IO (Either String [FingerprintCase])
      case loaded of
        Left reason -> expectationFailure reason
        Right cases -> mapM_ verify cases
  where
  verify item = fingerprint (caseName item) (caseArguments item) `shouldBe` caseFingerprint item
