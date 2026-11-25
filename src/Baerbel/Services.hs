module Baerbel.Services
  ( Service (..), Services (..), withServices, checkServices, withCallServices, dispatch ) where

import Control.Monad (void)
import Baerbel.Calendar (calendar, TimeZones(..))
import Baerbel.Config (Settings, setting, rule, toolDefinitions, booleanSetting)
import Baerbel.Contacts (contacts)
import Baerbel.Directory (LookupCaller, withDirectory)
import Baerbel.Error (Failure (..), Result, failure)
import Baerbel.Gmail (gmailSession)
import Baerbel.HTTP (Request (..), Send, authenticated, newSender, newSenderWith)
import qualified Baerbel.JSON
import Baerbel.Ledger (Ledger, withLedger, durable, lookupExisting)
import Baerbel.Maps (maps)
import Baerbel.NoteMail (NoteDelivery, noteSession)
import Baerbel.Worker (withWorker)
import Baerbel.Windguru (windguru)
import Baerbel.OAuth (loadCredentials, tokenSource)
import Baerbel.Profile (profile, ProfileState(..), ProfileStore(..), snapshot)
import Baerbel.Provider (Execute)
import Baerbel.ProviderConfig (ProfileRules)
import Baerbel.Store (profileStore)
import Baerbel.TimeZones (timeZones)
import Data.Aeson (Value, toJSON, FromJSON(..), ToJSON, genericParseJSON, genericToJSON)
import GHC.Generics (Generic)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)

-- Immutable descriptions and explicit effect capabilities, scoped by the caller.
data Service = Service
  { servicePrefix :: Text, serviceTools :: [Value], serviceExecute :: Execute }

data Services = Services
  { serviceFactories :: Factories
  , ownerProfileContext :: Maybe (IO (Result Text))
  , lookupPublicCaller :: LookupCaller
  , deliverOwnerNote :: Maybe (IO NoteDelivery)
  }
data Factory = Shared Service | Scoped (IO Service)
type Factories = [Factory]

sharedService :: Text -> [Value] -> Execute -> Factory
sharedService name schemas execute = Shared (Service name schemas execute)

scopedService :: Text -> [Value] -> IO Execute -> Factory
scopedService name schemas prepare = Scoped (Service name schemas <$> prepare)
type Accounts = Map Text Send

data AccountSpec = AccountSpec Text Text Text

accounts :: [AccountSpec]
accounts = [AccountSpec "gmail" "PHONE_ASSISTANT_GMAIL_TOKEN_FILE" "gmail",
            AccountSpec "calendar" "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE" "calendar",
            AccountSpec "contacts" "PHONE_ASSISTANT_CONTACTS_TOKEN_FILE" "contacts"]

withServices :: Settings -> (Services -> IO (Result a)) -> IO (Result a)
withServices settings action = either (pure . Left) prepare policy
  where
  policy = rule settings "http" "mutations"
  prepare mutations = do
    sender <- newSenderWith (mutationPolicy mutations)
    prepared <- prepareAccounts settings sender
    either (pure . Left) (bindGoogle settings sender action) prepared

data Mutation = Mutation
  { mutationMethod :: Text, mutationUrl :: Text, mutationDefiniteMinimum :: Int
  , mutationDefiniteMaximum :: Int, mutationUncertainStatuses :: [Int] } deriving (Generic)
instance FromJSON Mutation where parseJSON = genericParseJSON (Baerbel.JSON.jsonOptions 8)

mutationPolicy :: [Mutation] -> Request -> Maybe (Int -> Bool)
mutationPolicy mutations request = uncertainStatus <$> find matches mutations
  where
  uncertainStatus mutation status = status < mutationDefiniteMinimum mutation
    || status > mutationDefiniteMaximum mutation || status `elem` mutationUncertainStatuses mutation
  matches mutation = Text.encodeUtf8 (mutationMethod mutation) == requestMethod request && mutationUrl mutation == requestUrl request

prepareAccounts :: Settings -> Send -> IO (Result Accounts)
prepareAccounts settings sender = do
  results <- traverse prepare enabled
  pure (Map.fromList <$> sequence results)
  where
  enabled = filter configured accounts
  configured (AccountSpec _ key _) = not (Text.null (setting settings key))
  prepare = prepareAccount settings sender

prepareAccount :: Settings -> Send -> AccountSpec -> IO (Result (Text, Send))
prepareAccount settings sender (AccountSpec name key scopes) =
  either (pure . Left) prepare requirements
  where
  requirements = (,,) <$> rule settings "oauth" "refresh"
    <*> rule settings "oauth" scopes <*> rule settings "oauth" "allowed"
  prepare (endpoint, required, allowed) = do
    loaded <- loadCredentials endpoint (Set.fromList required) (Set.fromList allowed) path
    either (pure . Left) (bind endpoint required allowed) loaded
  bind endpoint required allowed credentials = do
    source <- tokenSource sender endpoint (Set.fromList required) (Set.fromList allowed) credentials
    let authorized request = prefixErrors name <$> authenticated source sender request
    pure (Right (name, authorized))
  path = Text.unpack (setting settings key)

prefixErrors :: Text -> Result a -> Result a
prefixErrors prefix = either (Left . addPrefix) Right
  where
  addPrefix err = err {failureCode = prefix <> "_" <> failureCode err}

bindGoogle :: Settings -> Send -> (Services -> IO (Result a)) -> Accounts -> IO (Result a)
bindGoogle settings sender action prepared = withOptionalLedger gmailPath withMail
  where
    gmailPath = enabledPath "gmail" "PHONE_ASSISTANT_GMAIL_STATE_FILE"
    calendarPath = enabledPath "calendar" "PHONE_ASSISTANT_CALENDAR_STATE_FILE"
    enabledPath name key
      | Map.member name prepared = Just (Text.unpack (setting settings key))
      | otherwise = Nothing
    withMail mailLedger = withOptionalLedger calendarPath (withCalendar mailLedger)
    withCalendar mailLedger calendarLedger = do
      stored <- optionalProfileStore settings
      let assembled = stored >>= assembleServices settings sender prepared mailLedger calendarLedger
      either (pure . Left) bindDirectory assembled
    bindDirectory services = either (pure . Left) withLookup directoryRules
      where withLookup rules = withDirectory rules directorySender (action . services)
    directoryRules = rule settings "incoming_contacts" "directory"
    directorySender
      | booleanSetting settings "INCOMING_SCREENING_ENABLED" == Right True = Map.lookup "contacts" prepared
      | otherwise = Nothing

assembleServices :: Settings -> Send -> Accounts -> Maybe Ledger -> Maybe Ledger -> Maybe ProfileStore -> Result (LookupCaller -> Services)
assembleServices settings sender prepared mailLedger calendarLedger store = do
  primary <- googleServices settings prepared mailLedger calendarLedger
  extras <- otherServices settings sender prepared store
  context <- profileContext settings store
  note <- noteDelivery settings prepared mailLedger
  Right (\lookupCaller -> Services (primary ++ extras) context lookupCaller note)

noteDelivery :: Settings -> Accounts -> Maybe Ledger -> Result (Maybe (IO NoteDelivery))
noteDelivery settings prepared mailLedger = case (Map.lookup "gmail" prepared, mailLedger) of
  (Just authorized, Just ledger) -> do
    rules <- rule settings "notes" "mail"
    mailRules <- rule settings "providers" "gmail"
    Right (Just (noteSession rules mailRules authorized (durable ledger) getCurrentTime))
  _ -> Right Nothing

withOptionalLedger :: Maybe FilePath -> (Maybe Ledger -> IO (Result a)) -> IO (Result a)
withOptionalLedger Nothing action = action Nothing
withOptionalLedger (Just path) action = withLedger path (action . Just)

googleServices :: Settings -> Accounts -> Maybe Ledger -> Maybe Ledger -> Result Factories
googleServices settings prepared mailLedger calendarLedger = do
  mailRules <- rule settings "providers" "gmail"
  calendarRules <- rule settings "providers" "calendar"
  let mail = make "gmail" "GMAIL_TOOLS" mailLedger (\schemas sender ledger ->
        scopedService "gmail" schemas (gmailSession mailRules sender (durable ledger) getCurrentTime))
      agenda = make "calendar" "CALENDAR_TOOLS" calendarLedger (\schemas sender ledger ->
        sharedService "calendar" schemas (calendar calendarRules sender (durable ledger) (lookupExisting ledger) timeZones))
  pure (mail ++ agenda)
  where
  make name group ledger build = case (Map.lookup name prepared, ledger) of
    (Just sender, Just state) -> [build (toolDefinitions settings group) sender state]
    _ -> []

otherServices :: Settings -> Send -> Accounts -> Maybe ProfileStore -> Result Factories
otherServices settings sender prepared store = concat <$> sequence
  [ storedProfile settings store
  , contactServices settings prepared
  , mapServices settings sender
  , windServices settings sender store
  ]

storedProfile :: Settings -> Maybe ProfileStore -> Result Factories
storedProfile _ Nothing = Right []
storedProfile settings (Just store) = do
  rules <- rule settings "providers" "profile"
  let schemas = toolDefinitions settings "PROFILE_TOOLS"
  Right [sharedService "profile" schemas (profile rules store getCurrentTime)]

contactServices :: Settings -> Accounts -> Result Factories
contactServices settings prepared = do
  rules <- rule settings "providers" "contacts"
  pure (maybe [] (build rules) (Map.lookup "contacts" prepared))
  where
  build rules sender = [scopedService "contacts" schemas (contacts rules sender)]
  schemas = toolDefinitions settings "CONTACTS_TOOLS"

mapServices :: Settings -> Send -> Result Factories
mapServices settings sender
  | Text.null key = Right []
  | otherwise = do
      rules <- rule settings "providers" "maps"
      routeRules <- rule settings "providers" "route"
      pure [scopedService "maps" schemas (maps rules routeRules (Text.replace key "[redacted]") authorized elevation getCurrentTime)]
  where
  key = setting settings "GOOGLE_MAPS_API_KEY"
  authorized request = sender (mapsAuthorization key request)
  schemas = toolDefinitions settings "MAPS_TOOLS"
  elevation = booleanSetting settings "PHONE_ASSISTANT_MAPS_ELEVATION" == Right True

mapsAuthorization :: Text -> Request -> Request
mapsAuthorization key request
  | requestMethod request == "POST" || places = request {requestHeaders = header : requestHeaders request}
  | otherwise = request {requestQuery = parameter : requestQuery request}
  where
  places = "https://places.googleapis.com/v1/places/" `Text.isPrefixOf` requestUrl request
  encoded = Text.encodeUtf8 key
  header = ("X-Goog-Api-Key", encoded)
  parameter = ("key", Just encoded)

withCallServices :: Services -> ([Service] -> IO (Result a)) -> IO (Result a)
withCallServices services action = traverse open (serviceFactories services) >>= scopeServices action
  where
    open (Shared service) = pure service
    open (Scoped prepare) = prepare

scopeServices :: ([Service] -> IO (Result a)) -> [Service] -> IO (Result a)
scopeServices action [] = action []
scopeServices action (service : rest)
  | servicePrefix service == "profile" = withWorker execute (\submit -> remaining service {serviceExecute = queued submit})
  | otherwise = remaining service
  where
    remaining current = scopeServices (action . (current :)) rest
    execute (name, arguments, ident) = serviceExecute service name arguments ident
    queued submit name arguments ident = submit (name, arguments, ident)

dispatch :: [Service] -> Execute
dispatch services name arguments ident = case filter matches services of
  service : _ -> serviceExecute service name arguments ident
  [] -> pure (Left (failure "tool_unavailable"))
  where
  matches service = (servicePrefix service <> "_") `Text.isPrefixOf` name

windServices :: Settings -> Send -> Maybe ProfileStore -> Result Factories
windServices settings sender store
  | not enabled = Right []
  | otherwise = do
      rules <- rule settings "providers" "windguru"
      let forecast = readForecastSet . sequence <$> traverse profileRead store
          prepare = windguru rules sender getCurrentTime (zonedInstant timeZones) forecast
      Right [scopedService "windguru" schemas prepare]
  where
    enabled = booleanSetting settings "PHONE_ASSISTANT_WINDGURU" == Right True
    schemas = toolDefinitions settings "WINDGURU_TOOLS" ++ setSchemas
    setSchemas = maybe [] (const (toolDefinitions settings "WINDGURU_SET_TOOL")) store

readForecastSet :: Result (Maybe ProfileState) -> Result Text
readForecastSet result = result >>= maybe missing select
  where
    select state = maybe missing Right (Map.lookup "windguru_forecast_set" (profileValues state))
    missing = Left (failure "windguru_forecast_set_not_configured")

optionalProfileStore :: Settings -> IO (Result (Maybe ProfileStore))
optionalProfileStore settings
  | Text.null (setting settings "PHONE_ASSISTANT_PROFILE_FILE") = pure (Right Nothing)
  | otherwise = fmap (Just <$>) (prepareProfileStore settings)

prepareProfileStore :: Settings -> IO (Result ProfileStore)
prepareProfileStore settings = either (pure . Left) prepare rules
  where
  rules = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
  prepare (profileRules, storeRules) = profileStore profileRules storeRules (Text.unpack (setting settings "PHONE_ASSISTANT_PROFILE_FILE"))

profileContext :: Settings -> Maybe ProfileStore -> Result (Maybe (IO (Result Text)))
profileContext _ Nothing = Right Nothing
profileContext settings (Just store) = do
  rules <- rule settings "providers" "profile"
  prefix <- rule settings "profile" "contextPrefix"
  Right (Just (Right . renderProfileContext rules prefix <$> profileRead store))

renderProfileContext :: ProfileRules -> Text -> Result ProfileState -> Text
renderProfileContext rules prefix result = prefix <> Baerbel.JSON.encodeReference value
  where value = either (toJSON . unavailableProfile) (\state -> snapshot rules state Nothing) result

checkServices :: Settings -> IO (Result ())
checkServices settings = do
  sender <- newSender
  accountsResult <- prepareAccounts settings sender
  profileResult <- optionalProfileStore settings
  pure (void accountsResult <* profileResult)

data UnavailableProfile = UnavailableProfile
  { unavailableStatus :: Text, unavailableError :: Text, unavailableProfileState :: Text } deriving (Generic)
instance ToJSON UnavailableProfile where toJSON = genericToJSON (Baerbel.JSON.jsonOptions 11)

unavailableProfile :: Failure -> UnavailableProfile
unavailableProfile err = UnavailableProfile "error" (failureCode err) "unavailable"
