module Baerbel.Core.OpeningHours (RawHours, Hours, openingHours, hoursAvailable) where

import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (camelOptions, jsonOptions)
import Baerbel.Core.ProviderConfig (MapsRules(..))
import Data.Aeson hiding (Result)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ZonedTime, defaultTimeLocale, formatTime, fromGregorianValid, parseTimeM)
import GHC.Generics (Generic)

data RawDate = RawDate {dateYear :: Integer, dateMonth :: Int, dateDay :: Int} deriving (Generic)
instance FromJSON RawDate where parseJSON = genericParseJSON (camelOptions 4)
data RawPoint = RawPoint
  { rawPointDay :: Int, rawPointHour :: Int, rawPointMinute :: Int
  , rawPointDate :: Maybe RawDate, rawPointTruncated :: Maybe Bool }
instance FromJSON RawPoint where
  parseJSON = withObject "opening time" (\value -> RawPoint <$> value .:! "day" .!= 0 <*> value .:! "hour" .!= 0
    <*> value .:! "minute" .!= 0 <*> value .:! "date" <*> value .:! "truncated")
data RawPeriod = RawPeriod {rawPeriodOpen :: RawPoint, rawPeriodClose :: Maybe RawPoint} deriving (Generic)
instance FromJSON RawPeriod where
  parseJSON = withObject "opening period" (\value -> RawPeriod <$> value .: "open" <*> value .:! "close")
newtype RawSpecial = RawSpecial {rawSpecialDate :: RawDate} deriving (Generic)
instance FromJSON RawSpecial where parseJSON = genericParseJSON (camelOptions 10)
data RawHours = RawHours
  { rawHoursOpenNow :: Maybe Bool, rawHoursWeekdayDescriptions :: Maybe [Text]
  , rawHoursPeriods :: Maybe [RawPeriod], rawHoursSpecialDays :: Maybe [RawSpecial]
  , rawHoursNextOpenTime :: Maybe Text, rawHoursNextCloseTime :: Maybe Text } deriving (Generic)
instance FromJSON RawHours where
  parseJSON = withObject "opening hours" (\value -> RawHours <$> value .:! "openNow" <*> value .:! "weekdayDescriptions"
    <*> value .:! "periods" <*> value .:! "specialDays" <*> value .:! "nextOpenTime" <*> value .:! "nextCloseTime")
data Point = Point {pointDay :: Int, pointHour :: Int, pointMinute :: Int, pointDate :: Maybe Text, pointTruncated :: Maybe Bool} deriving (Generic)
instance ToJSON Point where toJSON = genericToJSON ((jsonOptions 5) {omitNothingFields = True})
data Period = Period {periodOpen :: Point, periodClose :: Maybe Point} deriving (Generic)
instance ToJSON Period where toJSON = genericToJSON ((jsonOptions 6) {omitNothingFields = True})
newtype Special = Special {specialDate :: Text} deriving (Generic)
instance ToJSON Special where toJSON = genericToJSON (jsonOptions 7)
data Hours = Hours
  { hoursOpenNow :: Maybe Bool, hoursWeekdayDescriptions :: Maybe [Text]
  , hoursPeriods :: Maybe [Period], hoursSpecialDays :: Maybe [Special]
  , hoursNextOpenTime :: Maybe Text, hoursNextCloseTime :: Maybe Text
  , hoursDataTruncated :: Maybe Bool } deriving (Generic)
instance ToJSON Hours where toJSON = genericToJSON ((jsonOptions 5) {omitNothingFields = True})

dateText :: RawDate -> Result Text
dateText value
  | dateYear value < 1 || dateYear value > 9999 = invalid
  | otherwise = maybe invalid (Right . Text.pack . formatTime defaultTimeLocale "%F") parsed
  where
    parsed = fromGregorianValid (dateYear value) (dateMonth value) (dateDay value)
    invalid = Left (failure "maps_invalid_hours_response")

point :: RawPoint -> Result Point
point value
  | rawPointDay value < 0 || rawPointDay value > 6 = invalid
  | rawPointHour value < 0 || rawPointHour value > 23 = invalid
  | rawPointMinute value < 0 || rawPointMinute value > 59 = invalid
  | otherwise = Point (rawPointDay value) (rawPointHour value) (rawPointMinute value)
      <$> traverse dateText (rawPointDate value) <*> pure (rawPointTruncated value)
  where invalid = Left (failure "maps_invalid_hours_response")

period :: RawPeriod -> Result Period
period value = Period <$> point (rawPeriodOpen value) <*> traverse point (rawPeriodClose value)

timestamp :: Text -> Result Text
timestamp value = case parsed of
  Just _ -> Right value
  Nothing -> Left (failure "maps_invalid_hours_response")
  where
    normalized | "Z" `Text.isSuffixOf` value = Text.dropEnd 1 value <> "+00:00"
               | otherwise = value
    parsed = parseTimeM True defaultTimeLocale "%FT%T%Q%Ez" (Text.unpack normalized) :: Maybe ZonedTime

openingHours :: MapsRules -> (Text -> Text) -> Maybe RawHours -> Result (Maybe Hours)
openingHours _ _ Nothing = Right Nothing
openingHours rules redact (Just value) = do
  periods <- traverse (traverse period . take (mapsPeriodLimit rules)) (rawHoursPeriods value)
  specials <- traverse (traverse (fmap Special . dateText . rawSpecialDate) . take (mapsSpecialDayLimit rules)) (rawHoursSpecialDays value)
  nextOpen <- traverse (fmap redact . timestamp) (rawHoursNextOpenTime value)
  nextClose <- traverse (fmap redact . timestamp) (rawHoursNextCloseTime value)
  let descriptions = fmap (map (Text.take (mapsHoursDescriptionChars rules) . redact) . take (mapsWeekdayLimit rules)) (rawHoursWeekdayDescriptions value)
      output = Hours (rawHoursOpenNow value) descriptions periods specials nextOpen nextClose truncatedFlag
  Right (nonemptyHours output)
  where
    exceeds :: Int -> Maybe [a] -> Bool
    exceeds cap = maybe False ((> cap) . length)
    truncatedFlag | truncated = Just True
                  | otherwise = Nothing
    longDescription = any ((> mapsHoursDescriptionChars rules) . Text.length) (fromMaybe [] (rawHoursWeekdayDescriptions value))
    truncated = longDescription || exceeds (mapsWeekdayLimit rules) (rawHoursWeekdayDescriptions value)
      || exceeds (mapsPeriodLimit rules) (rawHoursPeriods value) || exceeds (mapsSpecialDayLimit rules) (rawHoursSpecialDays value)

nonemptyHours :: Hours -> Maybe Hours
nonemptyHours (Hours Nothing Nothing Nothing Nothing Nothing Nothing _) = Nothing
nonemptyHours hours = Just hours

hoursAvailable :: Hours -> Bool
hoursAvailable value = isJust (hoursOpenNow value) || isJust (hoursPeriods value)
  || maybe False (not . null) (hoursWeekdayDescriptions value)
  || maybe False (not . Text.null) (hoursNextOpenTime value) || maybe False (not . Text.null) (hoursNextCloseTime value)
