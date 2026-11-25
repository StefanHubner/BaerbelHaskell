module Baerbel.Core.PhoneNumber (PhoneMetadata, numberValidator, validateNumber) where

import Baerbel.Core.Config (Settings, rule)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.Routing (NumberValidator, normalizeDestination)
import Data.Aeson (FromJSON(..), genericParseJSON, withObject, (.:))
import Data.Foldable (toList)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Text.Regex.TDFA (Regex, defaultCompOpt, defaultExecOpt, makeRegexOptsM, matchTest, matchOnceText)

newtype Pattern = Pattern Regex
instance FromJSON Pattern where
  parseJSON value = do
    source <- parseJSON value
    let compiled = makeRegexOptsM defaultCompOpt defaultExecOpt (source :: String) :: Maybe Regex
    maybe (fail "invalid phone metadata pattern") (pure . Pattern) compiled

newtype PhoneMetadata = PhoneMetadata (Map String [Region])
instance FromJSON PhoneMetadata where
  parseJSON = withObject "PhoneMetadata" (fmap PhoneMetadata . (.: "countries"))

data Description = Description {descPattern :: Pattern, descLengths :: [Int]} deriving (Generic)
instance FromJSON Description where parseJSON = genericParseJSON (jsonOptions 4)

data Prefix = Prefix {prefixPattern :: Pattern, prefixReplacement :: Maybe (Int, Text)} deriving (Generic)
instance FromJSON Prefix where parseJSON = genericParseJSON (jsonOptions 6)

data Region = Region
  { regionLeading :: Maybe Pattern, regionGeneral :: Description
  , regionRestricted :: [Description], regionOrdinary :: [Description], regionPrefix :: Maybe Prefix
  } deriving (Generic)
instance FromJSON Region where parseJSON = genericParseJSON (jsonOptions 6)

data NumberClass = Invalid | Ordinary | Restricted deriving (Eq)

numberValidator :: Settings -> Result NumberValidator
numberValidator settings = validateNumber <$> rule settings "phone_numbers" "metadata"

validateNumber :: PhoneMetadata -> Text -> Result Text
validateNumber (PhoneMetadata countries) input = do
  canonical <- normalizeDestination input
  let digits = Text.unpack (Text.drop 1 canonical)
  (primary, regions, national) <- countryNumber countries digits
  region <- maybe invalid Right (selectRegion regions national)
  let classification = numberClass region national
      canonicalPrefix = unchanged primary national
  accept canonical classification canonicalPrefix
  where invalid = Left (failure "invalid_e164_number")

countryNumber :: Map String [Region] -> String -> Result (Region, [Region], String)
countryNumber countries digits = maybe invalid Right (listToMaybe candidates)
  where
    invalid = Left (failure "invalid_e164_number")
    candidates = [(primary, regions, drop size digits) | size <- [1..3],
      Just regions@(primary : _) <- [Map.lookup (take size digits) countries]]

selectRegion :: [Region] -> String -> Maybe Region
selectRegion [region] _ = Just region
selectRegion regions number = find selected regions
  where
    selected :: Region -> Bool
    selected region = case regionLeading region of
      Just leading -> matches leading number
      Nothing -> numberClass region number /= Invalid

numberClass :: Region -> String -> NumberClass
numberClass region number
  | not (matchesDescription number (regionGeneral region)) = Invalid
  | any (matchesDescription number) (regionRestricted region) = Restricted
  | any (matchesDescription number) (regionOrdinary region) = Ordinary
  | otherwise = Invalid

matchesDescription :: String -> Description -> Bool
matchesDescription number description = length number `elem` descLengths description && matches (descPattern description) number

matches :: Pattern -> String -> Bool
matches (Pattern regex) = matchTest regex

accept :: Text -> NumberClass -> Bool -> Result Text
accept _ Invalid _ = Left (failure "invalid_e164_number")
accept _ _ False = Left (failure "invalid_e164_number")
accept number Ordinary True = Right number
accept _ Restricted True = Left (failure "destination_class_not_permitted")

-- The original boundary rejects every repair performed by national-prefix parsing.
unchanged :: Region -> String -> Bool
unchanged region number = maybe True keep (regionPrefix region >>= (`replacePrefix` number))
  where
    description = regionGeneral region
    keep :: String -> Bool
    keep changed
      | matches (descPattern description) number && not (matches (descPattern description) changed) = True
      | length changed `notElem` descLengths description && any (>= length changed) (descLengths description) = True
      | otherwise = changed == number

replacePrefix :: Prefix -> String -> Maybe String
replacePrefix prefix number = do
  let Pattern regex = prefixPattern prefix
  (_, groups, suffix) <- matchOnceText regex number
  let captures = map fst (toList groups)
  pure (maybe suffix (replacement captures suffix) (prefixReplacement prefix))

replacement :: [String] -> String -> (Int, Text) -> String
replacement captures suffix (index, prefix) = case drop index captures of
  value : _ | not (null value) -> Text.unpack prefix <> value <> suffix
  _ -> suffix
