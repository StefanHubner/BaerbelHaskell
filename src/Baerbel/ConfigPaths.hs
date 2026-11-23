module Baerbel.ConfigPaths (validatePaths) where

import Control.Monad (join)
import Baerbel.Config (Settings(..), setting, rule, booleanSetting)
import Baerbel.Error (Result, failure, ioResult)
import Baerbel.JSON (jsonOptions)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, doesFileExist, getHomeDirectory)
import System.FilePath (isAbsolute, splitDirectories, (</>))
import Text.Regex.TDFA ((=~))

data PathRule = PathRule
  { pathKey :: Text, pathWhen :: Maybe Text, pathBoolean :: Bool, pathExisting :: Bool
  , pathTilde :: Bool, pathPattern :: Maybe Text, pathSeparate :: [Text] } deriving (Generic)
instance FromJSON PathRule where parseJSON = genericParseJSON (jsonOptions 4)

validatePaths :: FilePath -> FilePath -> Settings -> IO (Result Settings)
validatePaths globalPath userPath settings = either (pure . Left) prepare configured
  where
  configured = rule settings "validation" "paths"
  prepare rules = do
    home <- getHomeDirectory
    let values = Map.insert "$global" (Text.pack globalPath) (Map.insert "$user" (Text.pack userPath) (settingsValues settings))
        expanded = foldl (expand home) values rules
        active = filter (enabled settings) rules
    result <- ioResult "configuration_path_unavailable" (checkAll expanded active)
    pure (settings {settingsValues = Map.delete "$global" (Map.delete "$user" expanded)} <$ join result)

expand :: FilePath -> Map Text Text -> PathRule -> Map Text Text
expand home values rules
  | pathTilde rules && "~/" `Text.isPrefixOf` value = Map.insert key replacement values
  | otherwise = values
  where
  key = pathKey rules
  value = Map.findWithDefault "" key values
  replacement = Text.pack (home </> Text.unpack (Text.drop 2 value))

enabled :: Settings -> PathRule -> Bool
enabled settings rules = case pathWhen rules of
  Nothing -> not (Text.null (setting settings (pathKey rules)))
  Just key -> if pathBoolean rules then booleanSetting settings key == Right True else not (Text.null (setting settings key))

checkAll :: Map Text Text -> [PathRule] -> IO (Result ())
checkAll values rules = sequence_ <$> traverse (checkPath values) rules

checkPath :: Map Text Text -> PathRule -> IO (Result ())
checkPath values rules
  | not absolute || not lexical = pure invalid
  | otherwise = do
      exists <- doesFileExist path
      target <- canonicalizePath path
      others <- traverse canonicalizePath distinct
      pure (if (not (pathExisting rules) || exists) && target `notElem` others then Right () else invalid)
  where
  value = Map.findWithDefault "" (pathKey rules) values
  path = Text.unpack value
  absolute = isAbsolute path && ".." `notElem` splitDirectories path
  lexical = maybe True (\patternValue -> path =~ Text.unpack patternValue) (pathPattern rules)
  distinct = [Text.unpack other | key <- pathSeparate rules, Just other <- [Map.lookup key values], not (Text.null other)]
  invalid = Left (failure ("invalid_setting_path:" <> pathKey rules))
