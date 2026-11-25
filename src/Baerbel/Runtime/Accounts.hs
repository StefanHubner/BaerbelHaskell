module Baerbel.Runtime.Accounts (loadInstallation, AccountIngress(..), accountIngress, issuePin) where

import Baerbel.Core.Accounts
import Baerbel.Core.Config (Global, Settings, User(..), resolveSettings, rule, setting)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.JSON (encodeBytes)
import Baerbel.Core.Pin (encodePinHash, parsePinHash, verifyPin)
import Baerbel.Core.Telephony (TelephonyConfig(..), TelephonyRules(..))
import Baerbel.Runtime.AMI (AMI(..), connectAMI, field, incomingMatch, ownerMatch, variable)
import Baerbel.Runtime.Config (loadGlobal, readConfig)
import Baerbel.Runtime.ConfigPaths (validatePaths)
import Baerbel.Runtime.Effects (continue, mapResult, monotonicSeconds, withResult)
import Baerbel.Runtime.HTTP (newRawSender)
import Baerbel.Core.SMS (SMSCredentials(..), Delivery(Sent, Uncertain), classify, smsRequest)
import Baerbel.Runtime.Storage (atomicWrite)
import Baerbel.Runtime.Telephony (agiCommand, agiSession, readAgiLine, routeHome)
import Baerbel.Core.Routing (NumberValidator, homeEnabled)
import Baerbel.Core.Session (telephonyConfiguration)
import Baerbel.Core.Validation (validateSettings)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, stateTVar)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as Bytes
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Net
import System.Directory (canonicalizePath)
import Baerbel.Runtime.Error (ioResult)

loadInstallation :: FilePath -> FilePath -> IO (Result (Either Settings (Settings, Map Text Subscriber)))
loadInstallation globalPath path = do
  global <- loadGlobal globalPath
  config <- readConfig "user_config" path
  continue check ((,) <$> global <*> config)
  where
    check (global, config) = case installationAccounts config of
      Nothing -> fmap (fmap Left) (validate (baseSettings global config))
      Just _ -> continue (prepare (baseSettings global config)) (accountSettings global config)
    validate settings = continue (validatePaths globalPath path) (validateSettings settings)
    prepare shared accounts = runExceptT $ do
      ready <- ExceptT (fmap sequenceA (traverse prepareUser accounts))
      ExceptT (isolated ready)
      pure (Right (shared, ready))
    prepareUser sub = fmap (fmap (\settings -> sub {subscriberSettings = settings})) (validate (subscriberSettings sub))

baseSettings :: Global -> Installation -> Settings
baseSettings global = resolveSettings global . User . installationEnvironment

isolated :: Map Text Subscriber -> IO (Result ())
isolated accounts = mapResult (validateBindings accounts) paths
  where
    paths = ioResult "account_storage_unavailable" (traverse canonicalPaths accounts)
    canonicalPaths = traverse canonicalizePath . statePaths . subscriberSettings

statePaths :: Settings -> [FilePath]
statePaths settings = [T.unpack path | key <- accountStateKeys, let path = setting settings key, not (T.null path), active key]
  where
    active "PHONE_ASSISTANT_GMAIL_STATE_FILE" = configured "PHONE_ASSISTANT_GMAIL_TOKEN_FILE"
    active "PHONE_ASSISTANT_CALENDAR_STATE_FILE" = configured "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE"
    active key | key `elem` ["PHONE_ASSISTANT_SMS_STATE_FILE", "INCOMING_EVENTS_FILE", "INCOMING_EVENTS_STATE_FILE"] =
      setting settings "INCOMING_SMS_ENABLED" `elem` ["true", "1"]
    active _ = True
    configured key = not (T.null (setting settings key))

validateBindings :: Map Text Subscriber -> Map Text [FilePath] -> Result ()
validateBindings accounts paths = unless (unique belonging && one "HOME_PHONE_ENABLED" && one "INCOMING_SMS_ENABLED") invalid
  where
    belonging = concatMap distinct (Map.elems paths)
    distinct = Set.toList . Set.fromList
    one key = length (filter (enabled key) (Map.elems accounts)) <= 1
    enabled key sub = setting (subscriberSettings sub) key `elem` ["true", "1"]
    invalid = Left (failure "account_resources_overlap")

data AccountIngress = AccountIngress
  { identifyAccount :: Bool -> Text -> IO (Result Text)
  , accountAGI :: Socket -> IO (Result ())
  }

accountIngress :: Settings -> Map Text Subscriber -> NumberValidator -> IO (Result AccountIngress)
accountIngress shared accounts validate = continue prepare config
  where
    config = (,,) <$> telephonyConfiguration shared <*> traverse (telephonyConfiguration . subscriberSettings) accounts
      <*> rule shared "pin" "entryMicros"
    prepare (native, natives, duration) = do
      attempts <- newTVarIO Map.empty
      let route socket = agiSession (telephonyRules native) duration socket (dispatch natives attempts native socket)
      pure (Right (AccountIngress (findAccount native) route))
    dispatch natives attempts native socket headers = case Map.lookup "agi_network_script" headers of
      Just "account" -> authorize accounts attempts (telephonyRules native) socket headers
      Just "home-dial" | [cfg] <- filter (homeEnabled . routeSettings) (Map.elems natives) ->
        routeHome cfg validate socket headers
      Just "home-dial" -> except (Left (failure "home_channel_required"))
      _ -> except (Left (failure "unknown_agi_route"))

findAccount :: TelephonyConfig -> Bool -> Text -> IO (Result Text)
findAccount native owner ident = withResult connect amiClose lookupCall
  where
    connect = connectAMI (amiRules native) (amiUsername native) (amiPassword native) (const (pure ()))
    fields = "PA_CALL_ID,PA_OWNER_AUTHORIZED,PA_PERMISSION,PA_INCOMING_MODE,PA_ACCOUNT"
    lookupCall ami = mapResult select (amiAction ami "Status" [("Variables", fields)] (Just "StatusComplete"))
    select events = case filter matches events of
      [event] | validId (variable "PA_ACCOUNT" event) -> Right (variable "PA_ACCOUNT" event)
      _ -> Left (failure "call_account_required")
    matches event
      | owner = ownerMatch ident event
      | otherwise = incomingMatch ident event || announcement event
    announcement event = field "Event" event == "Status" && field "Context" event == "phone-assistant-public-answered"
      && field "Application" event == "AudioSocket" && variable "PA_CALL_ID" event == ident
      && variable "PA_PERMISSION" event == "handoff" && variable "PA_OWNER_AUTHORIZED" event == "0"

authorize :: Map Text Subscriber -> TVar (Map Text [Double]) -> TelephonyRules -> Socket -> Map Text Text -> ExceptT Failure IO ()
authorize accounts attempts rules socket headers = do
  (ident, sub) <- except (callerAccount accounts headers)
  ExceptT (reserveAttempt attempts ident)
  let encoded = setting (subscriberSettings sub) "PHONE_ASSISTANT_PRIVATE_PIN_HASH"
  pinHash <- except (parsePinHash encoded)
  lift (Net.sendAll socket "GET DATA agent-pass 8000 8\n")
  reply <- ExceptT (readAgiLine rules socket)
  digits <- except (pinDigitsReply reply)
  unless (verifyPin pinHash digits) (except denied)
  let command = "SET VARIABLE PA_ACCOUNT " <> ident
  ExceptT (agiCommand rules socket command "200 result=1")
  lift (atomically (modifyTVar' attempts (Map.delete ident)))

callerAccount :: Map Text Subscriber -> Map Text Text -> Result (Text, Subscriber)
callerAccount accounts headers = case Map.lookup "agi_context" headers of
  Just "baerbel-account-entry" -> maybe denied permitted candidate
  _ -> denied
  where
    phone = Map.findWithDefault "" "agi_arg_1" headers
    entry = Map.findWithDefault "" "agi_arg_2" headers
    candidate = accountForPhone accounts phone
    permitted pair@(ident, sub)
      | entry == "shared" && accountPlan (subscriberAccount sub) == Standard = Right pair
      | entry == ident && accountPlan (subscriberAccount sub) == Pro = Right pair
      | otherwise = denied

denied :: Result a
denied = Left (failure "account_authentication_failed")

pinDigitsReply :: Text -> Result Bytes.ByteString
pinDigitsReply reply = case T.stripPrefix "200 result=" reply of
  Just digits | T.length digits >= 4 && T.length digits <= 8 && T.all isDigit digits -> Right (Text.encodeUtf8 digits)
  _ -> denied

reserveAttempt :: TVar (Map Text [Double]) -> Text -> IO (Result ())
reserveAttempt state ident = do
  now <- monotonicSeconds
  atomically (stateTVar state (reserve now))
  where
    reserve now entries
      | length recent >= 5 = (denied, entries)
      | otherwise = (Right (), Map.insert ident (now : recent) entries)
      where recent = filter (> now - 900) (Map.findWithDefault [] ident entries)

issuePin :: FilePath -> FilePath -> Text -> IO (Result ())
issuePin globalPath path ident = runExceptT $ do
  global <- ExceptT (loadGlobal globalPath)
  config <- ExceptT (readConfig "user_config" path)
  let accounts = fromMaybe Map.empty (installationAccounts config)
  account <- except (maybe (Left (failure "unknown_account")) Right (Map.lookup ident accounts))
  digits <- lift (randomDigits 8)
  salt <- lift (getRandomBytes 16)
  let encoded = encodePinHash salt digits
      values = Map.insert "PHONE_ASSISTANT_PRIVATE_PIN_HASH" encoded (accountEnvironment account)
      updated = account {accountEnvironment = values}
      prepared = config {installationAccounts = Just (Map.insert ident updated accounts)}
  subscribers <- except (accountSettings global prepared)
  subscriber <- except (maybe (Left (failure "unknown_account")) Right (Map.lookup ident subscribers))
  let content = encodeBytes prepared
  ExceptT (sendPin path content (subscriberSettings subscriber) (accountPhone account) digits)

randomDigits :: Int -> IO Bytes.ByteString
randomDigits count = do
  sample <- getRandomBytes count
  let uniform = Bytes.map (\byte -> 48 + byte `mod` 10) (Bytes.filter (< 250) sample)
      missing = count - Bytes.length uniform
  case missing of
    0 -> pure uniform
    _ -> (uniform <>) <$> randomDigits missing

sendPin :: FilePath -> Bytes.ByteString -> Settings -> Text -> Bytes.ByteString -> IO (Result ())
sendPin path content settings phone digits = continue deliver request
  where
    username = setting settings "PHONE_ASSISTANT_SMS_USERNAME"
    password = setting settings "PHONE_ASSISTANT_SMS_PASSWORD"
    credentials = SMSCredentials username password phone
    body = "Your Bärbel telephone PIN is " <> Text.decodeUtf8 digits <> ". Enter it using your keypad."
    request
      | T.null username || T.null password = Left (failure "pin_delivery_unconfigured")
      | not (T.isPrefixOf "+447" phone && T.length phone == 13) = Left (failure "pin_delivery_requires_uk_mobile")
      | otherwise = rule settings "sms" "transport"
    deliver rules = runExceptT $ do
      let message = smsRequest rules credentials body
      sender <- lift newRawSender
      ExceptT (atomicWrite path content)
      result <- lift (sender message)
      except (delivery (either (const Uncertain) (classify rules) result))
    delivery result = case result of
      Sent -> Right ()
      Uncertain -> Left (Failure "pin_saved_sms_uncertain" True)
      _ -> Left (failure "pin_saved_sms_failed")
