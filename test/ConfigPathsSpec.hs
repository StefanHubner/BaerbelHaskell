module ConfigPathsSpec (spec) where

import Baerbel.Config (Global(..), Settings(..), User(..), resolveSettings, setting)
import Baerbel.ConfigPaths (validatePaths)
import Baerbel.Error (failure)
import Control.Monad (void)
import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createFileLink, getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

pathRule :: Bool -> Value -> [Text] -> Value
pathRule existing when separate = object
  [ "key" .= ("DATA" :: Text), "when" .= when, "boolean" .= True
  , "existing" .= existing, "tilde" .= True, "pattern" .= Null, "separate" .= separate ]

configured :: Value -> [(Text, Text)] -> Settings
configured rule values = resolveSettings global (User (Map.fromList values))
  where
    sections = Map.singleton "validation" (Map.singleton "paths" (toJSON [rule]))
    global = Global Map.empty sections Map.empty

expandedPath :: Expectation
expandedPath = do
  home <- getHomeDirectory
  let settings = configured (pathRule False Null []) [("DATA", "~/baerbel-path-check/nonexistent")]
      expected = Text.pack (home </> "baerbel-path-check/nonexistent")
  result <- validatePaths "/tmp/global.json" "/tmp/user.json" settings
  fmap (`setting` "DATA") result `shouldBe` Right expected
  fmap (Map.member "$global" . settingsValues) result `shouldBe` Right False
  fmap (Map.member "$user" . settingsValues) result `shouldBe` Right False

separatePaths :: Expectation
separatePaths = withSystemTempDirectory "baerbel-paths" $ \directory -> do
  let target = directory </> "data"
      alias = directory </> "alias"
      settings = configured (pathRule True Null ["OTHER"]) [("DATA", Text.pack target), ("OTHER", Text.pack alias)]
  writeFile target "data"
  createFileLink target alias
  result <- validatePaths "/tmp/global.json" "/tmp/user.json" settings
  void result `shouldBe` Left (failure "invalid_setting_path:DATA")

missingPath :: Expectation
missingPath = withSystemTempDirectory "baerbel-paths" $ \directory -> do
  let missing = Text.pack (directory </> "missing")
      settings = configured (pathRule True Null []) [("DATA", missing)]
  result <- validatePaths "/tmp/global.json" "/tmp/user.json" settings
  void result `shouldBe` Left (failure "invalid_setting_path:DATA")

conditionalPath :: Expectation
conditionalPath = do
  let rules = pathRule True (String "ENABLED") []
      disabled = configured rules [("DATA", "relative/path"), ("ENABLED", "false")]
      enabled = configured rules [("DATA", "relative/path"), ("ENABLED", "true")]
  unchecked <- validatePaths "/tmp/global.json" "/tmp/user.json" disabled
  fmap (`setting` "DATA") unchecked `shouldBe` Right "relative/path"
  checked <- validatePaths "/tmp/global.json" "/tmp/user.json" enabled
  void checked `shouldBe` Left (failure "invalid_setting_path:DATA")

spec :: Spec
spec = describe "Configuration filesystem boundary" $ do
  it "expands user paths without exposing comparison-only configuration keys" expandedPath
  it "rejects distinct settings that resolve to the same file" separatePaths
  it "rejects a missing required file" missingPath
  it "checks path syntax only when the configured feature is active" conditionalPath
