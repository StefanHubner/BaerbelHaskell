module Baerbel.Config
  ( Global (..), User (..), Settings (..), loadSettings, resolveSettings
  , setting, integerSetting, numericSetting, booleanSetting, toolDefinitions, rule, ruleSection
  ) where

import Baerbel.Error (Result, failure, ioResult)
import Baerbel.JSON (camelOptions, decodeBytes, decodeValue)
import Data.Aeson (FromJSON (..), Value, genericParseJSON, toJSON)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data Global = Global
  { globalDefaults :: Map Text Text
  , globalRules :: Map Text (Map Text Value)
  , globalTools :: Map Text [Value]
  } deriving (Show, Generic)

instance FromJSON Global where
  parseJSON = genericParseJSON (camelOptions 6)

newtype User = User { userEnvironment :: Map Text Text }
  deriving (Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (camelOptions 4)

data Settings = Settings
  { settingsGlobal :: Global
  , settingsValues :: Map Text Text
  }

loadSettings :: FilePath -> FilePath -> IO (Result Settings)
loadSettings globalPath userPath = do
  globalBytes <- ioResult "global_config_unavailable" (BS.readFile globalPath)
  userBytes <- ioResult "user_config_unavailable" (BS.readFile userPath)
  pure (resolveSettings <$> decodeGlobal globalBytes <*> decodeUser userBytes)
  where
  decodeGlobal = (>>= decodeBytes "global_config_invalid")
  decodeUser = (>>= decodeBytes "user_config_invalid")

resolveSettings :: Global -> User -> Settings
resolveSettings global user = Settings global values
  where
  values = Map.union (userEnvironment user) (globalDefaults global)

setting :: Settings -> Text -> Text
setting settings key = Map.findWithDefault "" key (settingsValues settings)

integerSetting :: Settings -> Text -> Result Int
integerSetting = readSetting

numericSetting :: Settings -> Text -> Result Double
numericSetting = readSetting

readSetting :: Read a => Settings -> Text -> Result a
readSetting settings key = maybe invalid Right parsed
  where
  value = Text.unpack (setting settings key)
  parsed = readMaybe value
  invalid = Left (failure ("invalid_setting:" <> key))

booleanSetting :: Settings -> Text -> Result Bool
booleanSetting settings key = choose (Text.toLower (setting settings key))
  where
  choose "true" = Right True
  choose "1" = Right True
  choose "false" = Right False
  choose "0" = Right False
  choose _ = Left (failure ("invalid_setting:" <> key))

toolDefinitions :: Settings -> Text -> [Value]
toolDefinitions settings key = Map.findWithDefault [] key catalogue
  where
  catalogue = globalTools (settingsGlobal settings)

rule :: FromJSON a => Settings -> Text -> Text -> Result a
rule settings section key = maybe missing (decodeValue "invalid_rule") value
  where
  sections = globalRules (settingsGlobal settings)
  values = Map.findWithDefault Map.empty section sections
  value = Map.lookup key values
  missing = Left (failure ("missing_rule:" <> section <> "." <> key))

ruleSection :: FromJSON a => Settings -> Text -> Result a
ruleSection settings section = maybe missing decode values
  where
  values = Map.lookup section (globalRules (settingsGlobal settings))
  decode = decodeValue "invalid_rule_section" . toJSON
  missing = Left (failure ("missing_rule_section:" <> section))
