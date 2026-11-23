{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Incoming
  ( Decision(..), HandoffRules(..), evaluateDecision, restrictTools, messageText
  , trustedEndpoint, callbackNumber, remainingBudget, handoffLine, callerDisplayName
  ) where

import Baerbel.AMI (Channel(..))
import Baerbel.Error (Result, failure)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON)
import Baerbel.JSON (camelOptions)
import GHC.Generics (Generic)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAscii, isDigit, isAlpha, ord)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

data Decision
  = Forward { decisionReason :: Text, callerName :: Text, callerSummary :: Text }
  | Decline { decisionReason :: Text, callerName :: Text, callerSummary :: Text }
  | LeaveMessage { callerName :: Text, callerMessage :: Text }
  deriving (Eq, Show)

messageText :: Int -> Bool -> Text -> Bool
messageText limit multiline text = not (T.null (T.strip text))
  && T.length text <= limit && T.all valid text
  where
    valid :: Char -> Bool
    valid char = (ord char >= 32 || multiline && char `elem` ['\n', '\t'])
      && not (ord char >= 127 && ord char <= 159)
      && not (ord char >= 0xd800 && ord char <= 0xdfff)

textField :: Text -> KM.KeyMap Value -> Result Text
textField key fields = case KM.lookup (K.fromText key) fields of
  Just (String value) -> Right value
  _ -> Left (failure "invalid_screening_decision")

optionalText :: Text -> Int -> KM.KeyMap Value -> Result Text
optionalText key limit fields = case KM.lookup (K.fromText key) fields of
  Nothing -> Right ""
  Just (String "") -> Right ""
  Just (String value) | messageText limit False value -> Right (T.strip value)
  _ -> Left (failure "invalid_screening_decision")

reasons :: [Text]
reasons = ["personal", "business", "delivery", "urgent", "uncertain", "marketing", "suspected_scam", "wrong_number", "caller_declined"]

screenDecision :: Bool -> KM.KeyMap Value -> Result Decision
screenDecision messageOnly fields = do
  choice <- textField "decision" fields
  reason <- textField "reason" fields
  name <- optionalText "caller_name" 40 fields
  summary <- optionalText "summary" 160 fields
  let allowedFields = keys fields `elem` [sort ["decision", "reason"], sort ["decision", "reason", "caller_name", "summary"]]
  if allowedFields && reason `elem` reasons then choose messageOnly choice reason name summary else Left (failure "invalid_screening_decision")

choose :: Bool -> Text -> Text -> Text -> Text -> Result Decision
choose _ "decline" reason name summary = Right (Decline reason name summary)
choose False "forward" reason name summary = Right (Forward reason name summary)
choose _ _ _ _ _ = Left (failure "invalid_screening_decision")

keys :: KM.KeyMap Value -> [Text]
keys = sort . map K.toText . KM.keys

messageDecision :: KM.KeyMap Value -> Result Decision
messageDecision fields = case (KM.lookup "caller_name" fields, KM.lookup "message" fields) of
  (Just (String name), Just (String body))
    | keys fields == ["caller_name", "message"] && messageText 100 False name && messageText 2000 True body ->
      Right (LeaveMessage (T.strip name) (T.strip body))
  _ -> Left (failure "invalid_message")

evaluateDecision :: Bool -> Bool -> Maybe Decision -> Text -> Value -> Result Decision
evaluateDecision _ _ (Just _) _ _ = Left (failure "screening_already_decided")
evaluateDecision True _ Nothing "take_message" (Object fields) = messageDecision fields
evaluateDecision _ _ Nothing "take_message" _ = Left (failure "invalid_message")
evaluateDecision _ messageOnly Nothing "screen_call" (Object fields) = screenDecision messageOnly fields
evaluateDecision _ _ Nothing _ _ = Left (failure "invalid_screening_decision")

restrictTools :: Bool -> Bool -> [Value] -> [Value]
restrictTools allowMessages messageOnly tools = map restrict (filter allowed tools)
  where
    allowed :: Value -> Bool
    allowed (Object fields) = KM.lookup "name" fields == Just (String "screen_call")
      || allowMessages && KM.lookup "name" fields == Just (String "take_message")
    allowed _ = False
    restrict :: Value -> Value
    restrict (Object fields)
      | messageOnly && KM.lookup "name" fields == Just (String "screen_call") = Object (updateField "parameters" restrictParameters fields)
    restrict value = value

restrictParameters :: Value -> Value
restrictParameters (Object fields) = Object (updateField "properties" restrictProperties fields)
restrictParameters value = value

restrictProperties :: Value -> Value
restrictProperties (Object fields) = Object (updateField "decision" restrictDecision fields)
restrictProperties value = value

restrictDecision :: Value -> Value
restrictDecision (Object fields) = Object (KM.insert "enum" (Array (V.singleton (String "decline"))) fields)
restrictDecision value = value

trustedEndpoint :: Channel -> Text -> Bool
trustedEndpoint channel endpoint = endpoint `elem` ["phone-assistant-aa", "phone-assistant-telematica"]
  && T.isPrefixOf ("PJSIP/" <> endpoint <> "-") (channelName channel)

callbackNumber :: Text -> Text -> Maybe Text
callbackNumber privacy number
  | privacy == "allowed" && valid = Just number
  | otherwise = Nothing
  where
    digits :: Text
    digits = T.drop 1 number
    valid :: Bool
    valid = T.isPrefixOf "+" number && T.length digits >= 7 && T.length digits <= 15
      && not (T.isPrefixOf "0" digits) && T.all (\char -> isAscii char && isDigit char) digits

remainingBudget :: Text -> Result Double
remainingBudget text = case readMaybe (T.unpack text) of
  Just value | lexical && value > 0 && value <= 60 -> Right value
  _ -> Left (failure "incoming_invalid_time_budget")
  where
    whole :: Text
    fractional :: Text
    (whole, fractional) = T.breakOn "." text
    digits :: Text -> Bool
    digits = T.all (\char -> isAscii char && isDigit char)
    lexical :: Bool
    lexical = T.length whole >= 1 && T.length whole <= 2 && digits whole
      && (T.null fractional || T.length fractional >= 2 && T.length fractional <= 4 && digits (T.drop 1 fractional))

data HandoffRules = HandoffRules
  { handoffIntroduction :: Text, handoffUnknown :: Text, handoffNamed :: Text
  , handoffDetail :: Text, handoffUnknownNames :: [Text]
  } deriving (Eq, Show, Generic)
instance FromJSON HandoffRules where parseJSON = genericParseJSON (camelOptions 7)

cleanHandoff :: Int -> Text -> Text
cleanHandoff limit = T.take limit . T.unwords . T.words . T.filter allowed
  where
    allowed :: Char -> Bool
    allowed char = ord char >= 32 && not (ord char >= 127 && ord char <= 159)
      && not (ord char >= 0xd800 && ord char <= 0xdfff)

handoffLine :: HandoffRules -> Text -> Text -> Bool -> Text
handoffLine rules name summary known = introduction <> detail
  where
    cleaned :: Text
    cleaned = cleanHandoff 40 name
    named :: Text
    named = if T.toCaseFold cleaned `elem` handoffUnknownNames rules then "" else cleaned
    who :: Text
    who
      | T.null named = handoffUnknown rules
      | known = named
      | otherwise = T.replace "{name}" named (handoffNamed rules)
    introduction :: Text
    introduction = T.replace "{who}" who (handoffIntroduction rules)
    summaryText :: Text
    summaryText = cleanHandoff 160 summary
    detail :: Text
    detail = if T.null summaryText || known then "" else T.replace "{detail}" (T.dropWhileEnd (`elem` (".!?…" :: String)) summaryText) (handoffDetail rules)

callerDisplayName :: Bool -> Text -> Maybe Text
callerDisplayName firstOnly input
  | T.null name || T.length name > 40 || not (T.all allowed name) = Nothing
  | otherwise = Just name
  where
    cleaned :: Text
    cleaned = T.unwords (T.words input)
    name :: Text
    name = if firstOnly then T.takeWhile (/= ' ') cleaned else cleaned
    allowed :: Char -> Bool
    allowed char = isAlpha char || char `elem` (" '-’" :: String)

updateField :: K.Key -> (Value -> Value) -> KM.KeyMap Value -> KM.KeyMap Value
updateField key transform = KM.fromList . map update . KM.toList
  where
    update :: (K.Key, Value) -> (K.Key, Value)
    update (name, value) = (name, if name == key then transform value else value)
