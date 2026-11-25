module Baerbel.Runtime.Config
  ( Global (..), User (..), Settings (..), loadSettings, loadFileSettings, loadGlobal, readGlobal, readConfig, resolveSettings
  , setting, integerSetting, numericSetting, booleanSetting, toolDefinitions, rule, ruleSection
  ) where

import Baerbel.Core.Config
    ( Settings(..),
      User(..),
      Global(..),
      resolveSettings,
      withPhoneNumbers,
      withInstructions,
      withToolSettings,
      withFileStorage,
      setting,
      integerSetting,
      numericSetting,
      booleanSetting,
      toolDefinitions,
      rule,
      ruleSection )
import Baerbel.Core.Error (failure)
import Baerbel.Core.JSON ( decodeBytes )
import Baerbel.Runtime.Error ( Result, ioResult )
import Baerbel.Runtime.Effects (continue)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Yaml as Yaml (decodeEither')
import System.FilePath (replaceFileName, takeDirectory, (</>))
import qualified Data.ByteString as BS ( ByteString, readFile )

loadSettings :: FilePath -> FilePath -> IO (Result Settings)
loadSettings globalPath userPath = loadGlobal globalPath >>= continue (loadFileSettings globalPath userPath)

loadFileSettings :: FilePath -> FilePath -> Global -> IO (Result Settings)
loadFileSettings globalPath userPath global = do
  storage <- readYaml "file_storage" storagePath
  user <- readConfig "user_config" userPath
  pure (resolveSettings . withFileStorage global <$> storage <*> user)
  where
    root = takeDirectory (takeDirectory globalPath)
    storagePath = root </> "deploy" </> "file-storage.yaml"

loadGlobal :: FilePath -> IO (Result Global)
loadGlobal path = do
  global <- readGlobal path
  let phonePath = replaceFileName path "phone-numbers.json"
  phones <- readConfig "phone_config" phonePath
  pure (withPhoneNumbers <$> phones <*> global)

readGlobal :: FilePath -> IO (Result Global)
readGlobal path = do
  global <- readYaml "global_config" path
  instructions <- readYaml "instructions" instructionPath
  tools <- traverse readProvider providers
  let configured = withInstructions <$> global <*> instructions
      catalogue = sequenceA tools
  pure (withToolSettings <$> configured <*> catalogue)
  where
    instructionPath = replaceFileName path "instructions.yaml"
    providers = ["assistant", "google", "windguru"]
    readProvider name = readYaml "tool_config" (toolDirectory </> (name <> ".yaml"))
    toolDirectory = takeDirectory path </> "tools"

readYaml :: FromJSON a => Text -> FilePath -> IO (Result a)
readYaml name = readWith name decode
  where decode code = either (const (Left (failure code))) Right . Yaml.decodeEither'

readConfig :: FromJSON a => Text -> FilePath -> IO (Result a)
readConfig name = readWith name decodeBytes

readWith :: Text -> (Text -> BS.ByteString -> Result a) -> FilePath -> IO (Result a)
readWith name decode path = do
  let unavailable = name <> "_unavailable"
      invalid = name <> "_invalid"
  bytes <- ioResult unavailable (BS.readFile path)
  pure (bytes >>= decode invalid)
