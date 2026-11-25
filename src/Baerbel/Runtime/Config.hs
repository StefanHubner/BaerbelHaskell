module Baerbel.Runtime.Config
  ( Global (..), User (..), Settings (..), loadSettings, resolveSettings
  , setting, integerSetting, numericSetting, booleanSetting, toolDefinitions, rule, ruleSection
  ) where

import Baerbel.Core.Config
    ( Settings(..),
      User(..),
      Global(..),
      resolveSettings,
      setting,
      integerSetting,
      numericSetting,
      booleanSetting,
      toolDefinitions,
      rule,
      ruleSection )
import Baerbel.Core.JSON ( decodeBytes )
import Baerbel.Runtime.Error ( Result, ioResult )
import qualified Data.ByteString as BS ( readFile )

loadSettings :: FilePath -> FilePath -> IO (Result Settings)
loadSettings globalPath userPath = do
  globalBytes <- ioResult "global_config_unavailable" (BS.readFile globalPath)
  userBytes <- ioResult "user_config_unavailable" (BS.readFile userPath)
  pure (resolveSettings <$> decodeGlobal globalBytes <*> decodeUser userBytes)
  where
  decodeGlobal = (>>= decodeBytes "global_config_invalid")
  decodeUser = (>>= decodeBytes "user_config_invalid")
