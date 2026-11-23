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
import Baerbel.Profile (profile, ProfileState(..), ProfileStore, snapshot)
import Baerbel.Provider (Execute)
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
  { serviceFactories :: [IO Service]
  , ownerProfileContext :: IO (Result Text)
  , lookupPublicCaller :: LookupCaller
  , deliverOwnerNote :: Maybe (IO NoteDelivery)
  }
type Factories = [IO Service]
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
    let authorized = authenticated source sender
    pure (Right (name, prefixErrors name authorized))
  path = Text.unpack (setting settings key)

prefixErrors :: Text -> Send -> Send
prefixErrors prefix sender request = fmap rename (sender request)
  where
  rename = either (Left . addPrefix) Right
  addPrefix err = err {failureCode = prefix <> "_" <> failureCode err}

bindGoogle :: Settings -> Send -> (Services -> IO (Result a)) -> Accounts -> IO (Result a)
bindGoogle settings sender action prepared =
  withOptionalLedger gmailPath withMail
  where
  gmailPath = enabledPath "gmail" "PHONE_ASSISTANT_GMAIL_STATE_FILE"
  calendarPath = enabledPath "calendar" "PHONE_ASSISTANT_CALENDAR_STATE_FILE"
  enabledPath name key = if Map.member name prepared then Just (Text.unpack (setting settings key)) else Nothing
  withMail mailLedger = withOptionalLedger calendarPath (withCalendar mailLedger)
  withCalendar mailLedger calendarLedger = do
    rest <- otherServices settings sender prepared
    context <- profileContext settings
    let assembled = assemble <$> googleServices settings prepared mailLedger calendarLedger <*> rest <*> delivery
        assemble primary extras note lookupCaller = Services (primary ++ extras) context lookupCaller note
    case (assembled, rule settings "incoming_contacts" "directory") of
      (Right services, Right directoryRules) -> withDirectory directoryRules directorySender (action . services)
      (Left err, _) -> pure (Left err)
      (_, Left err) -> pure (Left err)
    where
    directorySender = if booleanSetting settings "INCOMING_SCREENING_ENABLED" == Right True then Map.lookup "contacts" prepared else Nothing
    delivery = case (Map.lookup "gmail" prepared, mailLedger) of
      (Just authorized, Just ledger) -> do
        rules <- rule settings "notes" "mail"
        mailRules <- rule settings "providers" "gmail"
        pure (Just (noteSession rules mailRules authorized (durable ledger) getCurrentTime))
      _ -> Right Nothing

withOptionalLedger :: Maybe FilePath -> (Maybe Ledger -> IO (Result a)) -> IO (Result a)
withOptionalLedger Nothing action = action Nothing
withOptionalLedger (Just path) action = withLedger path (action . Just)

googleServices :: Settings -> Accounts -> Maybe Ledger -> Maybe Ledger -> Result Factories
googleServices settings prepared mailLedger calendarLedger = do
  mailRules <- rule settings "providers" "gmail"
  calendarRules <- rule settings "providers" "calendar"
  let mail = make "gmail" "GMAIL_TOOLS" mailLedger (\sender ledger -> gmailSession mailRules sender (durable ledger) getCurrentTime)
      agenda = make "calendar" "CALENDAR_TOOLS" calendarLedger (\sender ledger -> pure (calendar calendarRules sender (durable ledger) (lookupExisting ledger) timeZones))
  pure (mail ++ agenda)
  where
  make name group ledger build = case (Map.lookup name prepared, ledger) of
    (Just sender, Just state) -> [Service name (toolDefinitions settings group) <$> build sender state]
    _ -> []

otherServices :: Settings -> Send -> Accounts -> IO (Result Factories)
otherServices settings sender prepared = do
  stored <- storedProfile settings
  winds <- windServices settings sender
  pure ((\profileItems contactItems mapsItems windItems -> profileItems ++ contactItems ++ mapsItems ++ windItems) <$> stored <*> contactServices settings prepared <*> mapServices settings sender <*> winds)

storedProfile :: Settings -> IO (Result Factories)
storedProfile settings
  | Text.null path = pure (Right [])
  | otherwise = do
      store <- prepareProfileStore settings
      pure (build <$> store <*> rule settings "providers" "profile")
  where
  path = setting settings "PHONE_ASSISTANT_PROFILE_FILE"
  schemas = toolDefinitions settings "PROFILE_TOOLS"
  build store rules = [pure (Service "profile" schemas (profile rules store getCurrentTime))]

contactServices :: Settings -> Accounts -> Result Factories
contactServices settings prepared = do
  rules <- rule settings "providers" "contacts"
  pure (maybe [] (build rules) (Map.lookup "contacts" prepared))
  where
  build rules sender = [Service "contacts" schemas <$> contacts rules sender]
  schemas = toolDefinitions settings "CONTACTS_TOOLS"

mapServices :: Settings -> Send -> Result Factories
mapServices settings sender
  | Text.null key = Right []
  | otherwise = do
      rules <- rule settings "providers" "maps"
      routeRules <- rule settings "providers" "route"
      pure [Service "maps" schemas <$> maps rules routeRules (Text.replace key "[redacted]") authorized elevation getCurrentTime]
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
withCallServices services action = sequence (serviceFactories services) >>= scopeServices action

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

windServices :: Settings -> Send -> IO (Result Factories)
windServices settings sender
  | not enabled = pure (Right [])
  | otherwise = do
      prepared <- if Text.null path then pure (Right Nothing) else fmap (Just <$>) (prepareProfileStore settings)
      pure (build <$> rule settings "providers" "windguru" <*> prepared)
  where
  path = setting settings "PHONE_ASSISTANT_PROFILE_FILE"
  enabled = booleanSetting settings "PHONE_ASSISTANT_WINDGURU" == Right True
  schemas = toolDefinitions settings "WINDGURU_TOOLS" ++ [item | not (Text.null path), item <- toolDefinitions settings "WINDGURU_SET_TOOL"]
  readSet Nothing = pure (Left (failure "windguru_forecast_set_not_configured"))
  readSet (Just store) = fmap (>>= select) (store unchanged)
  build rules store = [Service "windguru" schemas <$> windguru rules sender getCurrentTime (zonedInstant timeZones) (readSet store)]
  unchanged value = Right (value, toJSON (profileValues value))
  select :: Value -> Result Text
  select value = case (Baerbel.JSON.decodeValue "windguru_forecast_set_not_configured" value :: Result (Map Text Text)) of
    Left err -> Left err
    Right values -> maybe (Left (failure "windguru_forecast_set_not_configured")) Right (Map.lookup "windguru_forecast_set" values)

prepareProfileStore :: Settings -> IO (Result ProfileStore)
prepareProfileStore settings = either (pure . Left) prepare rules
  where
  rules = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
  prepare (profileRules, storeRules) = profileStore profileRules storeRules (Text.unpack (setting settings "PHONE_ASSISTANT_PROFILE_FILE"))

profileContext :: Settings -> IO (IO (Result Text))
profileContext settings
  | Text.null path = pure (pure (Right ""))
  | otherwise = do
      store <- prepareProfileStore settings
      pure (either (pure . Left) load ((,,) <$> store <*> rule settings "providers" "profile" <*> rule settings "profile" "contextPrefix"))
  where
  path = setting settings "PHONE_ASSISTANT_PROFILE_FILE"
  load (store, rules, prefix) = do
    result <- store (\state -> Right (state, snapshot rules state Nothing))
    let value = either (toJSON . unavailableProfile) id result
    pure (Right (prefix <> Baerbel.JSON.encodeReference value))

checkServices :: Settings -> IO (Result ())
checkServices settings = do
  sender <- newSender
  accountsResult <- prepareAccounts settings sender
  profileResult <- if Text.null (setting settings "PHONE_ASSISTANT_PROFILE_FILE") then pure (Right ()) else fmap (const () <$>) (prepareProfileStore settings)
  pure (void accountsResult <* profileResult)

data UnavailableProfile = UnavailableProfile
  { unavailableStatus :: Text, unavailableError :: Text, unavailableProfileState :: Text } deriving (Generic)
instance ToJSON UnavailableProfile where toJSON = genericToJSON (Baerbel.JSON.jsonOptions 11)

unavailableProfile :: Failure -> UnavailableProfile
unavailableProfile err = UnavailableProfile "error" (failureCode err) "unavailable"
