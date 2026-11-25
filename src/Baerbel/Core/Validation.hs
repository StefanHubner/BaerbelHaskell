module Baerbel.Core.Validation (validateSettings) where

import Baerbel.Core.Config (Settings, setting, ruleSection, booleanSetting, numericSetting, integerSetting)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (jsonOptions)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Char (isAlpha)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

data Validation = Validation
  { validationBooleans :: [Text], validationIntegers :: Map Text Bounds
  , validationNumbers :: Map Text Bounds, validationEnums :: Map Text [Text]
  , validationRequired :: [Text], validationRejected :: Map Text [Text]
  , validationText :: [TextRule], validationDependencies :: [Dependency] } deriving (Generic)
instance FromJSON Validation where parseJSON = genericParseJSON (jsonOptions 10)
data Bounds = Bounds {boundsMinimum :: Double, boundsMaximum :: Maybe Double, boundsMultiple :: Maybe Int} deriving (Generic)
instance FromJSON Bounds where parseJSON = genericParseJSON (jsonOptions 6)
data TextRule = TextRule
  { textKey :: Text, textWhen :: Maybe Text, textOptional :: Bool, textMinimum :: Int
  , textMaximum :: Int, textPattern :: Maybe Text, textAlphabetic :: Bool } deriving (Generic)
instance FromJSON TextRule where parseJSON = genericParseJSON (jsonOptions 4)
data Dependency = Dependency
  { dependencyKey :: Text, dependencyBoolean :: Bool, dependencyRequired :: [Text]
  , dependencyFlags :: [Text], dependencyValues :: Map Text Text } deriving (Generic)
instance FromJSON Dependency where parseJSON = genericParseJSON (jsonOptions 10)

validateSettings :: Settings -> Result Settings
validateSettings settings = do
  rules <- ruleSection settings "validation"
  traverse_ (booleanSetting settings) (validationBooleans rules)
  traverse_ (integer settings) (Map.toList (validationIntegers rules))
  traverse_ (number settings) (Map.toList (validationNumbers rules))
  traverse_ (enumeration settings) (Map.toList (validationEnums rules))
  traverse_ (required settings) (validationRequired rules)
  traverse_ (rejected settings) (Map.toList (validationRejected rules))
  traverse_ (textValue settings) (validationText rules)
  traverse_ (dependency settings) (validationDependencies rules)
  pure settings

invalid :: Text -> Result a
invalid key = Left (failure ("invalid_setting:" <> key))

require :: Text -> Bool -> Result ()
require key valid
  | valid = Right ()
  | otherwise = invalid key

within :: Bounds -> Double -> Bool
within bounds value = not (isNaN value || isInfinite value) && value >= boundsMinimum bounds && maybe True (value <=) (boundsMaximum bounds)

integer :: Settings -> (Text, Bounds) -> Result ()
integer settings (key, bounds) = do
  value <- integerSetting settings key
  let multiple = maybe True (\step -> value `mod` step == 0) (boundsMultiple bounds)
  require key (multiple && within bounds (fromIntegral value))

number :: Settings -> (Text, Bounds) -> Result ()
number settings (key, bounds) = numericSetting settings key >>= require key . within bounds

enumeration :: Settings -> (Text, [Text]) -> Result ()
enumeration settings (key, values) = require key (setting settings key `elem` values)

required :: Settings -> Text -> Result ()
required settings key = require key (not (Text.null (Text.strip (setting settings key))))

rejected :: Settings -> (Text, [Text]) -> Result ()
rejected settings (key, values) = require key (setting settings key `notElem` values)

textValue :: Settings -> TextRule -> Result ()
textValue settings rules
  | not enabled || Text.null value && textOptional rules = Right ()
  | otherwise = require (textKey rules) (lengthValid && matches && alphabetic)
  where
  value = setting settings (textKey rules)
  enabled = maybe True (\key -> booleanSetting settings key == Right True) (textWhen rules)
  lengthValid = Text.length value >= textMinimum rules && Text.length value <= textMaximum rules
  matches = maybe True (\patternValue -> Text.unpack value =~ Text.unpack patternValue) (textPattern rules)
  alphabetic = not (textAlphabetic rules) || Text.any isAlpha value

dependency :: Settings -> Dependency -> Result ()
dependency settings rules
  | not enabled = Right ()
  | otherwise = traverse_ (required settings) (dependencyRequired rules)
      >> traverse_ flag (dependencyFlags rules) >> traverse_ equal (Map.toList (dependencyValues rules))
  where
  key = dependencyKey rules
  enabled | dependencyBoolean rules = booleanSetting settings key == Right True
          | otherwise = not (Text.null (setting settings key))
  flag name = require name (booleanSetting settings name == Right True)
  equal (name, value) = require name (setting settings name == value)
