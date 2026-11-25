module Baerbel.Runtime.Config
  ( Global (..), User (..), Settings (..), loadSettings, loadGlobal, readGlobal, readConfig, resolveSettings
  , setting, integerSetting, numericSetting, booleanSetting, toolDefinitions, rule, ruleSection
  ) where

import Baerbel.Core.Config
    ( Settings(..),
      User(..),
      Global(..),
      resolveSettings,
      withPhoneNumbers,
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
import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Yaml as Yaml (decodeEither')
import System.FilePath (replaceFileName)
import qualified Data.ByteString as BS ( ByteString, readFile )

loadSettings :: FilePath -> FilePath -> IO (Result Settings)
loadSettings globalPath userPath = do
  global <- loadGlobal globalPath
  user <- readConfig "user_config" userPath
  pure (resolveSettings <$> global <*> user)

loadGlobal :: FilePath -> IO (Result Global)
loadGlobal path = do
  global <- readGlobal path
  let phonePath = replaceFileName path "phone-numbers.json"
  phones <- readConfig "phone_config" phonePath
  pure (withPhoneNumbers <$> phones <*> global)

readGlobal :: FilePath -> IO (Result Global)
readGlobal = readWith "global_config" decode
  where decode code = either (const (Left (failure code))) Right . Yaml.decodeEither'

readConfig :: FromJSON a => Text -> FilePath -> IO (Result a)
readConfig name = readWith name decodeBytes

readWith :: Text -> (Text -> BS.ByteString -> Result a) -> FilePath -> IO (Result a)
readWith name decode path = do
  let unavailable = name <> "_unavailable"
      invalid = name <> "_invalid"
  bytes <- ioResult unavailable (BS.readFile path)
  pure (bytes >>= decode invalid)
