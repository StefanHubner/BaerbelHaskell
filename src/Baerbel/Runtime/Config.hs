module Baerbel.Runtime.Config
  ( Global (..), User (..), Settings (..), loadSettings, loadGlobal, readConfig, resolveSettings
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
import Baerbel.Core.JSON ( decodeBytes )
import Baerbel.Runtime.Error ( Result, ioResult )
import Data.Aeson (FromJSON)
import Data.Text (Text)
import System.FilePath (replaceFileName)
import qualified Data.ByteString as BS ( readFile )

loadSettings :: FilePath -> FilePath -> IO (Result Settings)
loadSettings globalPath userPath = do
  global <- loadGlobal globalPath
  user <- readConfig "user_config" userPath
  pure (resolveSettings <$> global <*> user)

loadGlobal :: FilePath -> IO (Result Global)
loadGlobal path = do
  global <- readConfig "global_config" path
  let phonePath = replaceFileName path "phone-numbers.json"
  phones <- readConfig "phone_config" phonePath
  pure (withPhoneNumbers <$> phones <*> global)

readConfig :: FromJSON a => Text -> FilePath -> IO (Result a)
readConfig name path = do
  let unavailable = name <> "_unavailable"
      invalid = name <> "_invalid"
  bytes <- ioResult unavailable (BS.readFile path)
  pure (bytes >>= decodeBytes invalid)
