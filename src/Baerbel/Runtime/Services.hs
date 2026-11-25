module Baerbel.Runtime.Services
  ( Service (..), Services (..), withServices, checkServices, withCallServices, dispatch ) where

import Baerbel.Core.Services
    ( AccountSpec(..),
      accounts,
      googleRequirements,
      mutationPolicy,
      prefixErrors,
      mapsAuthorization,
      readForecastSet,
      renderProfileContext )
import Baerbel.Core.TimeZones ( timeZones )
import Baerbel.Runtime.Calendar ( calendar, TimeZones(..) )
import Baerbel.Runtime.Config
    ( Settings, setting, rule, toolDefinitions, booleanSetting )
import Baerbel.Runtime.Contacts ( contacts )
import Baerbel.Runtime.Directory ( LookupCaller, withDirectory )
import Baerbel.Runtime.Error ( Result, failure )
import Baerbel.Runtime.Gmail ( gmailSession )
import Baerbel.Runtime.HTTP
    ( Send, newSender, newSenderWith, authenticated )
import Baerbel.Runtime.Ledger
    ( Ledger, withLedger, withAccountLedger, durable, lookupExisting )
import Baerbel.Runtime.Maps ( maps )
import Baerbel.Runtime.NoteMail ( NoteDelivery, noteSession )
import Baerbel.Runtime.OAuth ( loadCredentials, tokenSource )
import Baerbel.Core.OAuth ( checkCredentials )
import Baerbel.Runtime.Storage ( AccountStore )
import Baerbel.Runtime.UserDatabase ( accountMemory, googleCredentials )
import Baerbel.Runtime.Profile
    ( ProfileStore(profileRead), profile )
import Baerbel.Runtime.Provider ( Execute )
import Baerbel.Runtime.Store ( profileStore )
import Baerbel.Runtime.Windguru ( windguru )
import Baerbel.Runtime.Worker ( withWorker )
import Control.Monad ( void )
import Data.Aeson ( Value )
import Data.Map.Strict ( Map )
import Data.Maybe ( catMaybes )
import Data.Text ( Text )
import Data.Time ( getCurrentTime )
import qualified Baerbel.Runtime.Effects as Effects ( continue, mapResult )
import qualified Data.Map.Strict as Map
    ( lookup, member, fromList )
import qualified Data.Text as Text
    ( isPrefixOf, null, replace, unpack )

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

withServices :: Maybe AccountStore -> Settings -> (Services -> IO (Result a)) -> IO (Result a)
withServices store settings action = Effects.continue prepare policy
  where
  policy = rule settings "http" "mutations"
  prepare mutations = do
    sender <- newSenderWith (mutationPolicy mutations)
    prepared <- prepareAccounts store settings sender
    Effects.continue (bindGoogle store settings sender action) prepared

prepareAccounts :: Maybe AccountStore -> Settings -> Send -> IO (Result Accounts)
prepareAccounts store settings sender = do
  results <- traverse (prepareAccount store settings sender) accounts
  pure (sequence results >>= requireNoteConnection . Map.fromList . catMaybes)
  where
    requireNoteConnection prepared
      | booleanSetting settings "PHONE_ASSISTANT_OWNER_NOTES" == Right True && not (Map.member "gmail" prepared) =
          Left (failure "owner_notes_require_gmail")
      | otherwise = Right prepared

prepareAccount :: Maybe AccountStore -> Settings -> Send -> AccountSpec -> IO (Result (Maybe (Text, Send)))
prepareAccount store settings sender spec@(AccountSpec name key _) = Effects.continue prepare requirements
  where
    requirements = googleRequirements settings spec
    prepare (endpoint, required, allowed) = do
      loaded <- readCredentials endpoint required allowed
      Effects.continue (fmap sequenceA . traverse (bind endpoint required allowed)) loaded
    readCredentials endpoint required allowed = case store of
      Just scoped -> Effects.mapResult (traverse (checkCredentials endpoint required allowed)) (googleCredentials scoped name)
      Nothing | Text.null path -> pure (Right Nothing)
      Nothing -> fmap (fmap Just) (loadCredentials endpoint required allowed (Text.unpack path))
    bind endpoint required allowed credentials = do
      source <- tokenSource sender endpoint required allowed credentials
      let authorized request = prefixErrors name <$> authenticated source sender request
      pure (Right (name, authorized))
    path = setting settings key

bindGoogle :: Maybe AccountStore -> Settings -> Send -> (Services -> IO (Result a)) -> Accounts -> IO (Result a)
bindGoogle store settings sender action prepared = openLedger "gmail" "PHONE_ASSISTANT_GMAIL_STATE_FILE" withMail
  where
    openLedger name key next
      | not (Map.member name prepared) = next Nothing
      | otherwise = case store of
        Just scoped -> withAccountLedger scoped name (next . Just)
        Nothing -> withLedger (Text.unpack (setting settings key)) (next . Just)
    withMail mailLedger = openLedger "calendar" "PHONE_ASSISTANT_CALENDAR_STATE_FILE" (withCalendar mailLedger)
    withCalendar mailLedger calendarLedger = do
      stored <- optionalProfileStore store settings
      let assembled = stored >>= assembleServices settings sender prepared mailLedger calendarLedger
      Effects.continue bindDirectory assembled
    bindDirectory services = Effects.continue withLookup directoryRules
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

optionalProfileStore :: Maybe AccountStore -> Settings -> IO (Result (Maybe ProfileStore))
optionalProfileStore (Just scoped) settings = Effects.continue prepare (rule settings "profile" "store")
  where
    prepare rules = do
      let store = accountMemory rules scoped
      checked <- profileRead store
      pure (Just store <$ checked)
optionalProfileStore Nothing settings
  | Text.null path = pure (Right Nothing)
  | otherwise = Effects.continue prepare rules
  where
    path = setting settings "PHONE_ASSISTANT_PROFILE_FILE"
    rules = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
    prepare (profileRules, storeRules) = fmap (fmap Just) (profileStore profileRules storeRules (Text.unpack path))

profileContext :: Settings -> Maybe ProfileStore -> Result (Maybe (IO (Result Text)))
profileContext _ Nothing = Right Nothing
profileContext settings (Just store) = do
  rules <- rule settings "providers" "profile"
  prefix <- rule settings "profile" "contextPrefix"
  Right (Just (Right . renderProfileContext rules prefix <$> profileRead store))

checkServices :: Maybe AccountStore -> Settings -> IO (Result ())
checkServices store settings = do
  sender <- newSender
  accountsResult <- prepareAccounts store settings sender
  profileResult <- optionalProfileStore store settings
  pure (void accountsResult <* profileResult)
