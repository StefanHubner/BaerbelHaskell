module Baerbel.GoogleValidation (address, identifier, textValue) where

import Baerbel.Error (Result, failure)
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as Text

alphaNumeric :: Char -> Bool
alphaNumeric char = isAsciiLower char || isAsciiUpper char || isDigit char

address :: Text -> Result Text
address raw = case Text.splitOn "@" raw of
  [local, domain] | valid local domain -> Right (local <> "@" <> Text.toLower domain)
  _ -> Left (failure "invalid_email_address")
  where
    valid local domain = Text.length raw <= 254 && Text.length local <= 64
      && not (Text.null local) && Text.all localChar local
      && not ("." `Text.isPrefixOf` local || "." `Text.isSuffixOf` local || ".." `Text.isInfixOf` local)
      && length (Text.splitOn "." domain) >= 2 && all validLabel (Text.splitOn "." domain)
    localChar char = alphaNumeric char || char `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

validLabel :: Text -> Bool
validLabel value = Text.length value <= 63 && edges && Text.all interior value
  where
    edges = maybe False (alphaNumeric . fst) (Text.uncons value) && maybe False (alphaNumeric . snd) (Text.unsnoc value)
    interior char = alphaNumeric char || char == '-'

identifier :: Text -> Result Text
identifier value
  | Text.null value || Text.length value > 128 || not (Text.all allowed value) = Left (failure "invalid_message_id")
  | otherwise = Right value
  where allowed char = alphaNumeric char || char `elem` ("_-" :: String)

textValue :: Int -> Bool -> Text -> Result Text
textValue maximumLength isHeader value
  | Text.null (Text.strip value) || Text.length value > maximumLength = Left (failure "invalid_text_length")
  | Text.any invalid value = Left (failure "invalid_control_character")
  | otherwise = Right value
  where invalid char = char == '\DEL' || char < ' ' && (isHeader || char `notElem` ("\t\r\n" :: String))
