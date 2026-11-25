module Baerbel.Core.ConfigPaths where

import Baerbel.Core.Config
    ( Settings(..), setting, booleanSetting )
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( jsonOptions )
import Data.Aeson ( FromJSON(..), genericParseJSON )
import Data.Map.Strict ( Map )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import System.FilePath ( isAbsolute, splitDirectories, (</>) )
import Text.Regex.TDFA ( (=~) )
import qualified Data.Map.Strict as Map
    ( delete, lookup, findWithDefault, insert )
import qualified Data.Text as Text
    ( drop, isPrefixOf, null, pack, unpack )

data PathRule = PathRule
  { pathKey :: Text, pathWhen :: Maybe Text, pathBoolean :: Bool, pathExisting :: Bool
  , pathTilde :: Bool, pathPattern :: Maybe Text, pathSeparate :: [Text] } deriving (Generic)
instance FromJSON PathRule where parseJSON = genericParseJSON (jsonOptions 4)

data PathCheck = PathCheck
  { checkKey :: Text, checkFile :: FilePath, checkExisting :: Bool, checkDistinct :: [FilePath] }

preparePaths :: FilePath -> FilePath -> FilePath -> Settings -> [PathRule] -> (Settings, [Result PathCheck])
preparePaths globalPath userPath home settings rules = (prepared, checks)
  where
    values = Map.insert "$global" (Text.pack globalPath) (Map.insert "$user" (Text.pack userPath) (settingsValues settings))
    expanded = foldl (expand home) values rules
    prepared = settings {settingsValues = Map.delete "$global" (Map.delete "$user" expanded)}
    active = filter (enabled settings) rules
    checks = map (preparePath expanded) active

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
  Just key
    | pathBoolean rules -> booleanSetting settings key == Right True
    | otherwise -> not (Text.null (setting settings key))

preparePath :: Map Text Text -> PathRule -> Result PathCheck
preparePath values rules
  | not absolute || not lexical = Left (failure ("invalid_setting_path:" <> pathKey rules))
  | otherwise = Right (PathCheck (pathKey rules) path (pathExisting rules) distinct)
  where
    value = Map.findWithDefault "" (pathKey rules) values
    path = Text.unpack value
    absolute = isAbsolute path && ".." `notElem` splitDirectories path
    lexical = maybe True (\patternValue -> path =~ Text.unpack patternValue) (pathPattern rules)
    distinct = [Text.unpack other | key <- pathSeparate rules, Just other <- [Map.lookup key values], not (Text.null other)]

pathResult :: PathCheck -> Bool -> FilePath -> [FilePath] -> Result ()
pathResult check exists target others
  | checkExisting check && not exists || target `elem` others = Left (failure ("invalid_setting_path:" <> checkKey check))
  | otherwise = Right ()
