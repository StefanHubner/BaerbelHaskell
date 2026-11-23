{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Routing
  ( Permission(..), Provider(..), Route(..), RouteSettings(..)
  , NumberValidator, normalizeDestination, selectRoute, providerLabel
  , announcement, internationalWarning, routeValue
  ) where

import Baerbel.Error (Result, failure)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAscii, isDigit)
import Data.Text (Text)
import qualified Data.Text as T

data Permission = Owner | Screening deriving (Eq, Show)
data Provider = AA | Telematica | Home deriving (Eq, Show)
data Route = Route
  { routeNumber :: Text
  , routeProvider :: Provider
  , routeConfirmation :: Bool
  } deriving (Eq, Show)
data RouteSettings = RouteSettings
  { telematicaEnabled :: Bool
  , telematicaAvailable :: Bool
  , homeEnabled :: Bool
  } deriving (Eq, Show)

-- The capability must enforce libphonenumber validity, canonical equality and
-- ordinary fixed/mobile classes; a syntactic E.164 check is not sufficient.
type NumberValidator = Text -> Result Text

asciiDigit :: Char -> Bool
asciiDigit char = isAscii char && isDigit char

normalizeDestination :: Text -> Result Text
normalizeDestination value
  | T.length value > 64 = Left (failure "invalid_e164_number")
  | not prefix || T.null body || not (T.all permitted body) = Left (failure "international_number_required")
  | not valid = Left (failure "invalid_e164_number")
  | otherwise = Right canonical
  where
    prefix :: Bool
    prefix = T.isPrefixOf "+" value || T.isPrefixOf "00" value
    body :: Text
    body = if T.isPrefixOf "+" value then T.drop 1 value else T.drop 2 value
    permitted :: Char -> Bool
    permitted char = asciiDigit char || char `elem` (" ()-" :: String)
    digits :: Text
    digits = T.filter asciiDigit body
    canonical :: Text
    canonical = "+" <> digits
    valid :: Bool
    valid = T.length digits >= 2 && T.length digits <= 15
      && not (T.isPrefixOf "0" digits)

selectRoute :: NumberValidator -> RouteSettings -> Permission -> Text -> Result Route
selectRoute _ _ Screening _ = Left (failure "owner_required")
selectRoute _ settings Owner "home"
  | homeEnabled settings = Right (Route "home" Home False)
  | otherwise = Left (failure "home_phone_disabled")
selectRoute validate settings Owner value = do
  candidate <- normalizeDestination value
  canonical <- validate candidate
  chooseProvider settings canonical

chooseProvider :: RouteSettings -> Text -> Result Route
chooseProvider settings number
  | austrian && not available = Left (failure "telematica_unavailable_no_automatic_fallback")
  | austrian = Right (Route number Telematica False)
  | otherwise = Right (Route number AA international)
  where
    austrian :: Bool
    austrian = T.isPrefixOf "+43" number
    available :: Bool
    available = telematicaEnabled settings && telematicaAvailable settings
    international :: Bool
    international = not (T.isPrefixOf "+44" number)

providerLabel :: Provider -> Text
providerLabel AA = "A&A"
providerLabel Telematica = "Telematica"
providerLabel Home = "Home"

internationalWarning :: Text
internationalWarning = "That is an international call via AAISP and may cost more. Shall I continue?"

announcement :: Route -> Maybe Text -> Text
announcement route label
  | routeProvider route == Home = "Calling Home. I'll connect you now."
  | otherwise = "Calling " <> destination <> " via " <> provider <> ". I'll connect you now."
  where
    digits :: Text
    digits = T.intercalate " " (map T.singleton (T.unpack (T.takeEnd 3 (routeNumber route))))
    destination :: Text
    destination = maybe ("the number ending in " <> digits) (\name -> name <> ", ending in " <> digits) label
    provider :: Text
    provider = if routeProvider route == AA then "A and A" else providerLabel (routeProvider route)

routeValue :: Text -> Route -> Maybe Text -> Value
routeValue token route label = object
  [ "status" .= ("ok" :: Text), "result" .= result, "number" .= routeNumber route
  , "display_name" .= label, "provider" .= provider, "route_token" .= token
  , "provider_label" .= providerLabel (routeProvider route)
  , "announcement" .= announcement route label, "confirmation_label" .= confirmation
  , "requires_international_confirmation" .= routeConfirmation route
  , "international_warning" .= warning
  ]
  where
    result :: Text
    result = if routeConfirmation route then "confirmation_required" else "route_ready"
    provider :: Text
    provider = case routeProvider route of { AA -> "aa"; Telematica -> "telematica"; Home -> "home" }
    warning :: Text
    warning = if routeConfirmation route then internationalWarning else ""
    confirmation :: Text
    confirmation = if routeProvider route == Home then "Home" else "ending in " <> digits
    digits :: Text
    digits = T.intercalate " " (map T.singleton (T.unpack (T.takeEnd 3 (routeNumber route))))
