module Baerbel.Core.TimeZones (timeZones) where

import Baerbel.Core.Calendar (TimeZones (..))
import Baerbel.Core.Error (Result, failure)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time (LocalTime, UTCTime, ZonedTime, utcToZonedTime)
import Data.Time.Zones (TZ, localTimeToUTCTZ, timeZoneForUTCTime)
import Data.Time.Zones.All (tzByName)

timeZones :: TimeZones
timeZones = TimeZones local zoned

zone :: Text -> Result TZ
zone name = maybe missing Right (tzByName bytes)
  where
  bytes = Text.encodeUtf8 name
  missing = Left (failure "calendar_invalid_time_zone")

local :: Text -> LocalTime -> Result UTCTime
local name value = convert <$> zone name
  where convert timezone = localTimeToUTCTZ timezone value

zoned :: Text -> UTCTime -> Result ZonedTime
zoned name value = convert <$> zone name
  where
  convert timezone = utcToZonedTime offset value
    where offset = timeZoneForUTCTime timezone value
