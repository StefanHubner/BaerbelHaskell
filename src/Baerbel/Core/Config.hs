module Baerbel.Core.Config where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( camelOptions, decodeValue )
import Data.Aeson ( FromJSON(..), Options(rejectUnknownFields), Value(..), genericParseJSON, toJSON )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict ( Map )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import Text.Read ( readMaybe )
import qualified Data.Map.Strict as Map
    ( empty, insert, lookup, union, unionWith, findWithDefault, adjust )
import qualified Data.Text as Text ( toLower, unpack, replace, null )

data Global = Global
  { globalDefaults :: Map Text Text
  , globalRules :: Map Text (Map Text Value)
  , globalTools :: Map Text [Value]
  , globalInstallation :: Maybe Installation
  } deriving (Generic)

instance FromJSON Global where
  parseJSON = genericParseJSON ((camelOptions 6) {rejectUnknownFields = True}) . addToolDefault
    where
      addToolDefault (Object fields) = Object (KeyMap.union fields (KeyMap.singleton "tools" (Object KeyMap.empty)))
      addToolDefault value = value

data Instructions = Instructions
  { instructionsDefaults :: Map Text Text
  , instructionsRules :: Map Text (Map Text Value)
  , instructionsTools :: Map Text [Value]
  } deriving (Generic)

instance FromJSON Instructions where
  parseJSON = genericParseJSON ((camelOptions 12) {rejectUnknownFields = True})

withInstructions :: Global -> Instructions -> Global
withInstructions global content = global {globalDefaults = defaults, globalRules = rules, globalTools = tools}
  where
    defaults = Map.union (instructionsDefaults content) (globalDefaults global)
    rules = Map.unionWith (Map.unionWith mergeRule) (instructionsRules content) (globalRules global)
    tools = Map.union (instructionsTools content) (globalTools global)
    mergeRule (Object prompt) (Object policy) = Object (KeyMap.unionWith mergeRule prompt policy)
    mergeRule prompt _ = prompt

data Installation = Installation
  { installationEnvironment :: Map Text Text, installationDatabase :: FilePath }
  deriving (Generic)

instance FromJSON Installation where
  parseJSON = genericParseJSON ((camelOptions 12) {rejectUnknownFields = True})

newtype User = User { userEnvironment :: Map Text Text }
  deriving (Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (camelOptions 4)

data FileStorage = FileStorage
  { fileStorageDefaults :: Map Text Text, fileStoragePaths :: [Value] }
  deriving (Generic)

instance FromJSON FileStorage where
  parseJSON = genericParseJSON ((camelOptions 11) {rejectUnknownFields = True})

withFileStorage :: Global -> FileStorage -> Global
withFileStorage global storage = global {globalDefaults = defaults, globalRules = rules}
  where
    defaults = Map.union (globalDefaults global) (fileStorageDefaults storage)
    paths = Map.insert "paths" (toJSON (fileStoragePaths storage))
    rules = Map.adjust paths "validation" (globalRules global)

data Settings = Settings
  { settingsGlobal :: Global
  , settingsValues :: Map Text Text
  }

withPhoneNumbers :: Map Text Value -> Global -> Global
withPhoneNumbers phones global = global {globalRules = Map.insert "phone_numbers" phones (globalRules global)}

resolveSettings :: Global -> User -> Settings
resolveSettings global user = Settings global values
  where
  values = foldr (Map.adjust (renderOwnerName (Settings global scoped))) scoped templates
  scoped = Map.union (userEnvironment user) (globalDefaults global)
  templates = ["PHONE_ASSISTANT_GREETING", "PHONE_ASSISTANT_LIVE_INSTRUCTIONS"]

renderOwnerName :: Settings -> Text -> Text
renderOwnerName settings = Text.replace "{{owner_name}}" name
  where
    configured = setting settings "INCOMING_OWNER_NAME"
    name | Text.null configured = "there"
         | otherwise = configured

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
