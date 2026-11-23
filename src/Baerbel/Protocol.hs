{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Protocol
  ( SessionRole(..), SessionSettings(..), sessionConfig, greetingCommand
  , allowedTools, applyInstructions, audioCommand, toolOutputCommand, decodeAudio
  , startCommand, continueCommand, appendCommand, buildSession, buildSessionWithProfile, appendSessionContext
  , voiceIdentity, timeContext, gmailTimeContext, spokenWords, announcementComplete
  ) where

import Baerbel.Error (Result, failure)
import Baerbel.Config (Settings, rule, setting, integerSetting, booleanSetting)
import Baerbel.JSON (camelOptions)
import Control.Monad (foldM)
import Data.Char (isAlphaNum, generalCategory, GeneralCategory(..))
import Data.List (isInfixOf, isSuffixOf)
import Data.Text.Normalize (NormalizationMode(NFKD), normalize)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON, object, (.=), encode)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Time (UTCTime, ZonedTime, LocalTime(..), TimeOfDay(..), utctDay, localDay, addDays, utcToZonedTime, timeZoneName, formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Zones (TZ, utcToLocalTimeTZ, localTimeToUTCTZ, timeZoneForUTCTime)
import Data.Time.Zones.All (tzByName)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

data SessionRole = Owner | Restricted | Announcement deriving (Eq, Show)

data SessionSettings = SessionSettings
  { sessionRole :: SessionRole, voiceModel :: Text, voiceName :: Text
  , voiceInstructions :: Text, backendModel :: Text, backendInstructions :: Text
  , backendReasoning :: Text, backendTokenLimit :: Int, sessionTools :: [Value]
  , providerSampleRate :: Int
  } deriving (Eq, Show)

sessionConfig :: SessionSettings -> Value
sessionConfig settings = object
  [ "model" .= voiceModel settings, "instructions" .= voiceInstructions settings
  , "audio" .= audio, "delegation" .= delegation, "store" .= False
  ]
  where
    format = object ["type" .= ("audio/pcm" :: Text), "rate" .= providerSampleRate settings]
    audio = object ["format" .= format, "output" .= object ["voice" .= voiceName settings]]
    delegation = case sessionRole settings of
      Announcement -> object ["type" .= ("client" :: Text)]
      _ -> object ["type" .= ("responses" :: Text), "responses" .= backendConfig settings]

backendConfig :: SessionSettings -> Value
backendConfig settings = object (base ++ reasoning)
  where
    tools = allowedTools (sessionRole settings) (sessionTools settings)
    choice = case tools of
      [] -> "none" :: Text
      _ -> "auto"
    base = ["model" .= backendModel settings, "instructions" .= backendInstructions settings,
            "tools" .= tools, "tool_choice" .= choice, "max_output_tokens" .= backendTokenLimit settings,
            "parallel_tool_calls" .= False]
    supportsReasoning = any (`T.isPrefixOf` backendModel settings) ["gpt-5", "gpt-6"]
    reasoning = ["reasoning" .= object ["effort" .= backendReasoning settings] |
                 supportsReasoning && not (T.null (backendReasoning settings))]

allowedTools :: SessionRole -> [Value] -> [Value]
allowedTools Owner tools = tools
allowedTools Announcement _ = []
allowedTools Restricted tools = filter restricted tools
  where
    restricted (Object fields) = KM.lookup "name" fields `elem` [Just (String "screen_call"), Just (String "take_message")]
    restricted _ = False

-- Templates and policy prose come from the global configuration, never user data.
greetingCommand :: Text -> Text -> Maybe Value
greetingCommand template greeting
  | T.null greeting = Nothing
  | otherwise = Just (object ["type" .= ("session.instructions.append" :: Text),
                              "event_id" .= ("phone_greeting" :: Text), "delegation_id" .= Null,
                              "content" .= T.replace "{greeting}" greeting template])

applyInstructions :: Text -> [(Text, Text)] -> Text
applyInstructions = foldl replace
  where
    replace instructions (key, value) = T.replace key value instructions

startCommand :: Value -> Value
startCommand session = object ["type" .= ("session.start" :: Text), "event_id" .= ("phone_start" :: Text), "session" .= session]

appendCommand :: Text -> Text -> Value
appendCommand kind content = object ["type" .= kind, "delegation_id" .= Null, "content" .= content]

continueCommand :: Value
continueCommand = object ["type" .= ("response.create" :: Text)]

audioCommand :: BS.ByteString -> Value
audioCommand pcm = object ["type" .= ("session.input_audio.append" :: Text), "audio" .= encoded]
  where
    encoded = TE.decodeUtf8 (Base64.encode pcm)

toolOutputCommand :: Text -> Value -> Value
toolOutputCommand ident result = object ["type" .= ("response.item.create" :: Text), "item" .= item]
  where
    output = TE.decodeUtf8 (BL.toStrict (encode result))
    item = object ["type" .= ("function_call_output" :: Text), "call_id" .= ident, "output" .= output]

decodeAudio :: Value -> Result BS.ByteString
decodeAudio (Object fields) = case KM.lookup "delta" fields of
  Just (String text) | T.length text <= 64000 -> validateDecoded (Base64.decode (TE.encodeUtf8 text))
  _ -> Left (failure "invalid_live_audio")
decodeAudio _ = Left (failure "invalid_live_audio")

validateDecoded :: Either String BS.ByteString -> Result BS.ByteString
validateDecoded (Right bytes) | even (BS.length bytes) = Right bytes
validateDecoded _ = Left (failure "invalid_live_audio")

data VoicePreset = VoicePreset
  { presetVoice :: Text, presetInstructions :: Text, presetStyle :: Text
  } deriving (Eq, Show, Generic)
instance FromJSON VoicePreset where parseJSON = genericParseJSON (camelOptions 6)

type Instructions = (Text, Text)

buildSession :: Settings -> UTCTime -> [Value] -> Result SessionSettings
buildSession settings now = buildSessionWithProfile settings now ""

buildSessionWithProfile :: Settings -> UTCTime -> Text -> [Value] -> Result SessionSettings
buildSessionWithProfile settings now profile tools = do
  base <- initialInstructions settings tools
  integrated <- foldM (integrate settings now profile names) base integrationSpecs
  final <- finishInstructions settings now names integrated
  makeOwnerSession settings tools final
  where
    names = toolNames tools

makeOwnerSession :: Settings -> [Value] -> Instructions -> Result SessionSettings
makeOwnerSession settings tools (voice, backend) = do
  tokens <- integerSetting settings "OPENAI_LIVE_MAX_OUTPUT_TOKENS"
  pure SessionSettings
    { sessionRole = Owner, voiceModel = setting settings "OPENAI_LIVE_MODEL"
    , voiceName = setting settings "OPENAI_LIVE_VOICE", voiceInstructions = voice
    , backendModel = setting settings "OPENAI_CHAT_MODEL", backendInstructions = backend
    , backendReasoning = setting settings "OPENAI_CHAT_REASONING_EFFORT"
    , backendTokenLimit = tokens, sessionTools = tools, providerSampleRate = 24000
    }

initialInstructions :: Settings -> [Value] -> Result Instructions
initialInstructions settings tools = do
  identity <- voiceIdentity settings
  instructions <- configuredInstruction settings "PHONE_ASSISTANT_LIVE_INSTRUCTIONS" "live_instructions"
  help <- rule settings "live" "HELP_INSTRUCTIONS"
  background <- rule settings "live" "BACKGROUND_INSTRUCTIONS"
  backend <- rule settings "live" "LIVE_BACKEND_INSTRUCTIONS"
  let initial = (identity <> "\n\n" <> instructions <> help <> background, backend <> help)
      voiceTest = "voice_test" `elem` toolNames tools
      pin = not (T.null (setting settings "PHONE_ASSISTANT_PRIVATE_PIN_HASH"))
  tested <- applyRule settings voiceTest prependBoth "VOICE_TEST_INSTRUCTIONS" initial
  applyRule settings pin appendBoth "PIN_INSTRUCTIONS" tested

voiceIdentity :: Settings -> Result Text
voiceIdentity settings = do
  presets <- rule settings "voice_catalogue" "VOICE_PRESETS"
  active <- rule settings "voice_catalogue" "DEFAULT_ACTIVE_PRESET"
  legacy <- rule settings "voice_catalogue" "LEGACY_NAME_PRONUNCIATION_INSTRUCTIONS"
  friendly <- rule settings "voice_catalogue" "FRIENDLY_IDENTITY_INSTRUCTIONS"
  normal <- rule settings "config" "ASSISTANT_IDENTITY_INSTRUCTIONS"
  let selected = setting settings "PHONE_ASSISTANT_VOICE_PRESET"
      voice = setting settings "OPENAI_LIVE_VOICE"
      preset = Map.lookup selected presets
  pure (identityFor active selected voice legacy friendly normal preset)

identityFor :: Text -> Text -> Text -> Text -> Text -> Text -> Maybe VoicePreset -> Text
identityFor active selected voice legacy friendly normal preset = case preset of
  Just choice | presetVoice choice == voice && selected == active ->
    T.intercalate "\n\n" [friendly, presetInstructions choice, presetStyle choice]
  Just choice | presetVoice choice == voice ->
    T.replace legacy (presetInstructions choice) normal <> optionalStyle (presetStyle choice)
  _ -> normal
  where
    optionalStyle text
      | T.null text = ""
      | otherwise = " " <> text

configuredInstruction :: Settings -> Text -> Text -> Result Text
configuredInstruction settings key fallback
  | T.null custom = rule settings "settings" fallback
  | otherwise = Right custom
  where
    custom = setting settings key

appendBoth :: Text -> Instructions -> Instructions
appendBoth text (voice, backend) = (voice <> text, backend <> text)

appendBackend :: Text -> Instructions -> Instructions
appendBackend text (voice, backend) = (voice, backend <> text)

prependBoth :: Text -> Instructions -> Instructions
prependBoth text (voice, backend) = (text <> "\n\n" <> voice, text <> "\n\n" <> backend)

applyRule :: Settings -> Bool -> (Text -> Instructions -> Instructions) -> Text -> Instructions -> Result Instructions
applyRule _ False _ _ current = Right current
applyRule settings True transform key current = (`transform` current) <$> rule settings "live" key

data IntegrationSpec = IntegrationSpec Text Text Text Text Text Text Text

integrationSpecs :: [IntegrationSpec]
integrationSpecs =
  [ IntegrationSpec "profile_" "local_profile" "PROFILE_INSTRUCTIONS" "PROFILE_RULE" "" "" ""
  , IntegrationSpec "windguru_" "windguru" "WINDGURU_INSTRUCTIONS" "" "windguruHeader" "windguru_instructions" "WINDGURU"
  , IntegrationSpec "gmail_" "google_gmail" "GMAIL_INSTRUCTIONS" "GMAIL_SEND_RULE" "gmailHeader" "gmail_instructions" "GMAIL"
  , IntegrationSpec "owner_note_" "owner_notes" "" "OWNER_NOTE_INSTRUCTIONS" "" "" ""
  , IntegrationSpec "calendar_" "google_calendar" "CALENDAR_INSTRUCTIONS" "CALENDAR_CREATION_RULE" "calendarHeader" "calendar_instructions" "CALENDAR"
  , IntegrationSpec "maps_" "google_maps" "MAPS_INSTRUCTIONS" "" "mapsHeader" "maps_instructions" "MAPS"
  , IntegrationSpec "contacts_" "google_contacts" "CONTACTS_INSTRUCTIONS" "" "contactsHeader" "contacts_instructions" "CONTACTS"
  ]

integrate :: Settings -> UTCTime -> Text -> [Text] -> Instructions -> IntegrationSpec -> Result Instructions
integrate settings now profile names current spec@(IntegrationSpec prefix section _ priority _ _ _)
  | not (any (prefix `T.isPrefixOf`) names) = Right current
  | otherwise = do
      instructions <- integrationInstructions settings spec
      context <- integrationContext settings now profile section
      let appended = appendBoth (instructions <> context) current
          result | T.null priority = Right appended
                 | otherwise = (`prependBoth` appended) <$> rule settings ("integrations." <> section) priority
      result

integrationInstructions :: Settings -> IntegrationSpec -> Result Text
integrationInstructions settings (IntegrationSpec _ section key _ header fallback service)
  | T.null key = Right ""
  | T.null header = rule settings ("integrations." <> section) key
  | otherwise = do
      base <- rule settings ("integrations." <> section) key
      separator <- rule settings "protocol" header
      let settingKey = "PHONE_ASSISTANT_" <> service <> "_INSTRUCTIONS"
      custom <- configuredInstruction settings settingKey fallback
      pure (base <> separator <> custom)

integrationContext :: Settings -> UTCTime -> Text -> Text -> Result Text
integrationContext settings now _ "google_gmail" = gmailTimeContext settings now
integrationContext _ _ profile "local_profile" = Right profile
integrationContext _ _ _ _ = Right ""

finishInstructions :: Settings -> UTCTime -> [Text] -> Instructions -> Result Instructions
finishInstructions settings now names current = do
  direct <- directInstructions settings ("direct_call" `elem` names) current
  readOnly <- applyRule settings (not mutations) appendBackend "READ_ONLY_INSTRUCTIONS" direct
  web <- booleanSetting settings "PHONE_ASSISTANT_WEB_SEARCH"
  let availableRule | web = Right readOnly
                    | otherwise = appendConfigured settings "webDisabled" readOnly
  available <- availableRule
  timed <- (`appendBoth` available) <$> timeContext settings now
  applyRule settings ("end_call" `elem` names) prependBoth "END_CALL_INSTRUCTIONS" timed
  where
    mutations = any (`elem` names) ["gmail_send", "gmail_reply", "calendar_create", "profile_set", "profile_forget",
                                   "owner_note_send", "direct_call", "end_call"]

directInstructions :: Settings -> Bool -> Instructions -> Result Instructions
directInstructions _ False current = Right current
directInstructions settings True (voice, backend) = do
  voiceRules <- rule settings "live" "DIRECT_CALL_VOICE_INSTRUCTIONS"
  backendRules <- rule settings "live" "DIRECT_CALL_INSTRUCTIONS"
  confirmation <- rule settings "live" "DIRECT_CALL_CONFIRMATION_RULE"
  pure (prependBoth confirmation (voice <> voiceRules, backend <> backendRules))

appendConfigured :: Settings -> Text -> Instructions -> Result Instructions
appendConfigured settings key current = (`appendBoth` current) <$> rule settings "protocol" key

toolNames :: [Value] -> [Text]
toolNames = foldr collect []
  where
    collect (Object fields) names = case KM.lookup "name" fields of
      Just (String name) -> name : names
      _ -> names
    collect _ names = names

configuredZone :: Settings -> Result TZ
configuredZone settings = do
  name <- rule settings "protocol" "timeZone"
  let encoded = TE.encodeUtf8 name
  maybe (Left (failure "invalid_time_zone")) Right (tzByName encoded)

timeContext :: Settings -> UTCTime -> Result Text
timeContext settings now = do
  zone <- configuredZone settings
  template <- rule settings "protocol" "timeContext"
  let offset = timeZoneForUTCTime zone now
      local = utcToZonedTime offset now
      values = [("{utc_date}", T.pack (show (utctDay now))), ("{local_time}", isoTime local),
                ("{zone}", T.pack (timeZoneName offset))]
  pure (applyInstructions template values)

gmailTimeContext :: Settings -> UTCTime -> Result Text
gmailTimeContext settings now = do
  zone <- configuredZone settings
  template <- rule settings "protocol" "gmailTimeContext"
  let day = localDay (utcToLocalTimeTZ zone now)
      midnight = LocalTime day midnightTime
      tomorrow = LocalTime (addDays 1 day) midnightTime
      start = localTimeToUTCTZ zone midnight
      end = localTimeToUTCTZ zone tomorrow
      values = [("{midnight}", isoTime (zoned zone start)), ("{tomorrow}", isoTime (zoned zone end)),
                ("{after}", epochSeconds start), ("{before}", epochSeconds end)]
  pure (applyInstructions template values)
  where
    midnightTime = TimeOfDay 0 0 0
    zoned zone stamp = utcToZonedTime (timeZoneForUTCTime zone stamp) stamp
    epochSeconds stamp = T.pack (show (floor (utcTimeToPOSIXSeconds stamp) :: Integer))

isoTime :: ZonedTime -> Text
isoTime stamp = date <> prefix <> ":" <> suffix
  where
    date = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" stamp)
    offset = T.pack (formatTime defaultTimeLocale "%z" stamp)
    (prefix, suffix) = T.splitAt 3 offset

appendSessionContext :: Text -> SessionSettings -> SessionSettings
appendSessionContext context settings = settings
  { voiceInstructions = voiceInstructions settings <> context
  , backendInstructions = backendInstructions settings <> context }


spokenWords :: Text -> [Text]
spokenWords = normalizeLabel . T.words . T.map punctuation . contractions . T.filter visible . normalize NFKD . T.toCaseFold
  where
    visible char = generalCategory char `notElem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]
    contractions = T.replace "can't" "cannot" . T.replace "i'm" "i am" . T.replace "i'll" "i will" . T.replace "’" "'"
    punctuation char
      | isAlphaNum char = char
      | otherwise = ' '

normalizeLabel :: [Text] -> [Text]
normalizeLabel ("g" : "p" : "t" : "live" : number : rest)
  | number `elem` ["one", "1"] = "gpt" : "live" : "1" : normalizeLabel rest
normalizeLabel ("gpt" : "live" : number : rest)
  | number `elem` ["one", "1"] = "gpt" : "live" : "1" : normalizeLabel rest
normalizeLabel (word : rest) = word : normalizeLabel rest
normalizeLabel [] = []

announcementComplete :: Bool -> Text -> Text -> Bool
announcementComplete marker ending transcript = not (null suffix) && matching suffix wordsOf
  where
    suffix = spokenWords ending
    wordsOf = spokenWords transcript
    matching | marker = isInfixOf
             | otherwise = isSuffixOf
