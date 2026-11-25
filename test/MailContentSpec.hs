module MailContentSpec (spec) where

import Baerbel.Error (Result, failure)
import Baerbel.JSON (decodeBytes, jsonOptions)
import Baerbel.MailContent (decodeCharset, decodeWords)
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Test.Hspec

data CodecCase = CodecCase
  { caseLabel :: Text, caseCharset :: Text, caseBytes :: Text, caseHeader :: Text
  , caseBody :: Text, caseDecodedHeader :: Maybe Text } deriving (Generic)
instance FromJSON CodecCase where parseJSON = genericParseJSON (jsonOptions 4)

fixtures :: IO (Result [CodecCase])
fixtures = fmap (decodeBytes "mail_codec_fixtures") (Bytes.readFile "test/fixtures/mail-codecs.json")

checkFixture :: CodecCase -> Expectation
checkFixture fixture = case Base64.decode (Text.encodeUtf8 (caseBytes fixture)) of
  Left issue -> expectationFailure issue
  Right bytes -> do
    decodeCharset (caseCharset fixture) bytes `shouldReturn` caseBody fixture
    let expected = maybe (Left (failure "invalid_message_header")) Right (caseDecodedHeader fixture)
    decodeWords (caseHeader fixture) `shouldReturn` expected

spec :: Spec
spec = describe "mail codec equivalence" $ do
  loaded <- runIO fixtures
  case loaded of
    Left issue -> it "loads original codec fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ codecExample cases
  it "decodes valid UTF-8 scalar boundaries in bodies and strict headers" $ do
    let value = Text.pack ['\0', '\x7f', '\x80', '\x7ff', '\x800', '\xd7ff', '\xe000', '\xffff', '\x10000', '\x10ffff']
        bytes = Text.encodeUtf8 value
        header = encodedHeader bytes
    decodeCharset "UTF_8" bytes `shouldReturn` value
    decodeWords header `shouldReturn` Right value
  it "retains ICU grouping for interrupted UTF-8 and unknown-charset fallback" $
    mapM_ checkInterrupted [[224,160], [237,159], [240,144,128], [244,143,191]]
  it "keeps stateful charset converters independent across concurrent messages" $ do
    let header = "=?iso-2022-jp?B?GyRCRnxLXDhsGyhC?="
        inputs = replicate 100 header
    values <- mapConcurrently decodeWords inputs
    values `shouldBe` replicate 100 (Right "日本語")
  it "does not accept converter options from encoded header charset names" $
    decodeWords "=?UTF-8//IGNORE?B?/2E=?=" `shouldReturn` Left (failure "invalid_message_header")
  it "preserves text around adjacent encoded words" $
    decodeWords "Re: =?utf-8?Q?Gr=C3=BC=C3=9Fe?= =?utf-8?B?IQ==?=" `shouldReturn` Right "Re: Grüße!"
  where codecExample fixture = it (Text.unpack (caseLabel fixture)) (checkFixture fixture)

encodedHeader :: Bytes.ByteString -> Text
encodedHeader bytes = "=?utf-8?B?" <> Text.decodeUtf8 (Base64.encode bytes) <> "?="

checkInterrupted :: [Word] -> Expectation
checkInterrupted prefix = do
  let bytes = Bytes.pack (map fromIntegral (prefix <> [65]))
      header = encodedHeader bytes
  decodeCharset "utf-8" bytes `shouldReturn` "\xfffd\&A"
  decodeCharset "unknown-test" bytes `shouldReturn` "\xfffd\&A"
  decodeWords header `shouldReturn` Left (failure "invalid_message_header")
