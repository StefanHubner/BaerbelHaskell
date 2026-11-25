module Baerbel.Core.TimeFormat (isoUtc, isoZoned) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time

isoUtc :: UTCTime -> Text
isoUtc value = localText (utcToLocalTime utc value) <> "Z"

isoZoned :: ZonedTime -> Text
isoZoned value = localText (zonedTimeToLocalTime value) <> offset
  where offset = Text.pack (formatTime defaultTimeLocale "%Ez" value)

localText :: LocalTime -> Text
localText value = date <> fractional
  where
    date = Text.pack (formatTime defaultTimeLocale "%FT%T" value)
    micros = floor (toRational (todSec (localTimeOfDay value)) * 1000000) `mod` 1000000 :: Integer
    digits = Text.justifyRight 6 '0' (Text.pack (show micros))
    fractional | micros == 0 = ""
               | otherwise = "." <> digits
