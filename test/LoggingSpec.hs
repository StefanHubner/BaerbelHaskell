{-# LANGUAGE OverloadedStrings #-}
module LoggingSpec (spec) where

import Baerbel.Runtime.Logging
import Control.Exception (bracket)
import Data.Aeson (Value(..), object, (.=), eitherDecodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (getTemporaryDirectory, removeFile, renameFile)
import System.IO (openTempFile, hClose)
import Test.Hspec

spec :: Spec
spec = do
  it "logs rejected owner-note metadata without recording content or exposing it publicly" $ do
    let event = object ["type" .= ("phone.note_rejected" :: Text), "action" .= ("send" :: Text),
          "state" .= ("active" :: Text), "error" .= ("notes_pending" :: Text), "text" .= ("private note" :: Text)]
        expected = [("owner_note_rejected", ["action" .= ("send" :: Text), "state" .= ("active" :: Text), "error" .= ("notes_pending" :: Text)])]
    projectEvent True event `shouldBe` expected
    projectEvent False event `shouldBe` []
  it "logs transcript text only for owner sessions and never tool arguments/results" $ do
    let transcript = object ["type" .= ("session.input_transcript.delta" :: Text), "delta" .= ("private transcript" :: Text)]
        tool = object ["type" .= ("phone.tool" :: Text), "tool" .= ("gmail_send" :: Text),
          "status" .= ("ok" :: Text), "arguments" .= ("private draft" :: Text), "result" .= ("private receipt" :: Text)]
    projectEvent False transcript `shouldBe` []
    map fst (projectEvent True transcript) `shouldBe` ["live_transcript"]
    case projectEvent True tool of
      [(name, fields)] -> do
        name `shouldBe` "live_tool"
        lookup "arguments" fields `shouldBe` Nothing
        lookup "result" fields `shouldBe` Nothing
      events -> expectationFailure (show events)
  it "retains tool failure codes without logging private arguments or results" $ do
    let event = object ["type" .= ("phone.tool" :: Text), "tool" .= ("windguru_forecast" :: Text),
          "status" .= ("error" :: Text), "error" .= ("windguru_forecast_unavailable" :: Text),
          "arguments" .= ("private request" :: Text), "result" .= ("private response" :: Text)]
    case projectEvent True event of
      [(_, fields)] -> do
        lookup "error" fields `shouldBe` Just (String "windguru_forecast_unavailable")
        lookup "arguments" fields `shouldBe` Nothing
        lookup "result" fields `shouldBe` Nothing
      events -> expectationFailure (show events)
  it "keeps backend lifecycle metadata but excludes public source citations" $ do
    let event = object ["type" .= ("response.event" :: Text), "delegation_id" .= ("delegation" :: Text),
          "event" .= object ["type" .= ("response.output_text.annotation.added" :: Text),
            "annotation" .= object ["type" .= ("url_citation" :: Text), "url" .= ("https://example.test" :: Text), "title" .= ("Source" :: Text)]]]
    projectEvent False event `shouldBe` []
    map fst (projectEvent True event) `shouldBe` ["live_source"]
  it "reopens an externally rotated append log and preserves each event once" $ bracket create cleanup $ \path -> do
    result <- withLogging (Just path) $ \report -> do
      report "before_rotation" []
      renameFile path (path ++ ".old")
      report "after_rotation" []
      pure (Right ())
    result `shouldBe` Right ()
    old <- eventsFrom (path ++ ".old")
    current <- eventsFrom path
    old `shouldBe` [Right (String "before_rotation")]
    current `shouldBe` [Right (String "after_rotation")]
  where
    create :: IO FilePath
    create = do
      root <- getTemporaryDirectory
      (path, handle) <- openTempFile root "baerbel-logging-"
      hClose handle
      pure path
    cleanup :: FilePath -> IO ()
    cleanup path = removeFile path >> removeFile (path ++ ".old")

eventsFrom :: FilePath -> IO [Either String Value]
eventsFrom path = map decodeEvent . BSC.lines <$> BS.readFile path
  where
    decodeEvent :: BS.ByteString -> Either String Value
    decodeEvent bytes = do
      value <- eitherDecodeStrict' bytes
      case value of
        Object fields -> Right (fromMaybe Null (KM.lookup "event" fields))
        _ -> Left "Expected event object"
