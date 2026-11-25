module Baerbel.Runtime.Accounts (loadInstallation, AccountIngress(..), accountIngress, issuePin, connectGoogle) where

import Baerbel.Core.Accounts
import Baerbel.Core.Config (Global(..), Installation(..), Settings, User(..), resolveSettings, rule, setting)
import Baerbel.Core.Error (Failure(..), Result, failure)
import Baerbel.Core.Pin (encodePinHash)
import Baerbel.Core.Telephony (TelephonyConfig(..), TelephonyRules(..))
import Baerbel.Runtime.AMI (AMI(..), connectAMI, field, incomingMatch, ownerMatch, variable)
import Baerbel.Runtime.Config (loadGlobal, readConfig)
import Baerbel.Runtime.ConfigPaths (validatePaths)
import Baerbel.Runtime.Effects (continue, mapResult, withResult)
import Baerbel.Runtime.HTTP (newRawSender)
import Baerbel.Core.SMS (SMSCredentials(..), Delivery(Sent, Uncertain), classify, smsRequest)
import Baerbel.Runtime.OAuth (loadCredentials)
import Baerbel.Runtime.Storage (AccountStore(..))
import Baerbel.Runtime.UserDatabase (readAccounts, saveGoogleCredentials, updatePin)
import Baerbel.Core.Services (googleAccount, googleRequirements)

import Baerbel.Runtime.Telephony (agiCommand, agiSession, routeHome)
import Baerbel.Core.Routing (NumberValidator, homeEnabled)
import Baerbel.Core.Session (telephonyConfiguration)
import Baerbel.Core.Validation (validateSettings)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as Bytes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Network.Socket (Socket)

loadInstallation :: FilePath -> Maybe FilePath -> IO (Result (Either Settings (Settings, FilePath, Map Text Subscriber)))
loadInstallation path local = do
  global <- loadGlobal path
  continue configure global
  where
    configure global = case (globalInstallation global, local) of
      (Just installation, Nothing) -> configureAccounts (global {globalInstallation = Nothing}) installation
      (Nothing, Just config) -> fmap (fmap Left) (configureLocal global config)
      _ -> pure (Left (failure "invalid_configuration_scope"))
    configureLocal global config = do
      user <- readConfig "user_config" config
      continue (validate config . resolveSettings global) user
    configureAccounts global installation = runExceptT $ do
      let database = installationDatabase installation
          values = installationEnvironment installation
      accounts <- ExceptT (readAccounts database)
      subscribers <- except (accountSettings global values accounts)
      ready <- ExceptT (fmap sequenceA (traverse prepare subscribers))
      pure (Right (resolveSettings global (User values), database, ready))
    validate config settings = continue (validatePaths path config) (validateSettings settings)
    prepare sub = fmap (fmap (\settings -> sub {subscriberSettings = settings})) (validate path (subscriberSettings sub))

data AccountIngress = AccountIngress
  { identifyAccount :: Bool -> Text -> IO (Result Text)
  , accountAGI :: Socket -> IO (Result ())
  }

accountIngress :: Settings -> Map Text Subscriber -> NumberValidator -> Result AccountIngress
accountIngress shared accounts validate = prepare <$> config
  where
    config = (,) <$> telephonyConfiguration shared <*> traverse (telephonyConfiguration . subscriberSettings) accounts
    prepare (native, natives) = AccountIngress (findAccount native) route
      where
      rules = telephonyRules native
      route socket = agiSession rules (nativeAGIMicros rules) socket (dispatch natives rules socket)
    dispatch natives rules socket headers = case Map.lookup "agi_network_script" headers of
      Just "account" -> identifyCaller accounts rules socket headers
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

identifyCaller :: Map Text Subscriber -> TelephonyRules -> Socket -> Map Text Text -> ExceptT Failure IO ()
identifyCaller accounts rules socket headers = do
  ident <- except (callerAccount accounts headers)
  let command = "SET VARIABLE PA_ACCOUNT " <> ident
  ExceptT (agiCommand rules socket command "200 result=1")

callerAccount :: Map Text Subscriber -> Map Text Text -> Result Text
callerAccount accounts headers = case Map.lookup "agi_context" headers of
  Just "baerbel-account-entry" -> maybe denied permitted candidate
  _ -> denied
  where
    phone = Map.findWithDefault "" "agi_arg_1" headers
    entry = Map.findWithDefault "" "agi_arg_2" headers
    candidate = accountForPhone accounts phone
    permitted (ident, sub)
      | entry == "shared" && accountPlan (subscriberAccount sub) == Standard = Right ident
      | entry == ident && accountPlan (subscriberAccount sub) == Pro = Right ident
      | otherwise = denied

denied :: Result a
denied = Left (failure "account_authentication_failed")

withAccount :: FilePath -> Text -> (AccountStore -> Subscriber -> IO (Result ())) -> IO (Result ())
withAccount globalPath ident action = do
  loaded <- loadInstallation globalPath Nothing
  continue (uncurry action) (loaded >>= selected)
  where
    selected (Right (_, database, subscribers)) = case Map.lookup ident subscribers of
      Just sub -> Right (AccountStore database ident, sub)
      Nothing -> Left (failure "unknown_account")
    selected (Left _) = Left (failure "user_database_required")

issuePin :: FilePath -> Text -> IO (Result ())
issuePin globalPath ident = withAccount globalPath ident issue
  where
    issue store sub = do
      digits <- randomDigits 4
      salt <- getRandomBytes 16
      let encoded = encodePinHash salt digits
          phone = accountPhone (subscriberAccount sub)
          save = updatePin store encoded
      sendPin save (subscriberSettings sub) phone digits

connectGoogle :: FilePath -> Text -> Text -> FilePath -> IO (Result ())
connectGoogle globalPath ident service token = withAccount globalPath ident connect
  where
    connect store sub = continue load (googleAccount service >>= googleRequirements (subscriberSettings sub))
      where
        load (rules, required, allowed) = do
          credentials <- loadCredentials rules required allowed token
          continue (saveGoogleCredentials store service) credentials

randomDigits :: Int -> IO Bytes.ByteString
randomDigits count = do
  sample <- getRandomBytes count
  let uniform = Bytes.map (\byte -> 48 + byte `mod` 10) (Bytes.filter (< 250) sample)
      missing = count - Bytes.length uniform
  case missing of
    0 -> pure uniform
    _ -> (uniform <>) <$> randomDigits missing

sendPin :: IO (Result ()) -> Settings -> Text -> Bytes.ByteString -> IO (Result ())
sendPin save settings phone digits = continue deliver request
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
      ExceptT save
      result <- lift (sender message)
      except (delivery (either (const Uncertain) (classify rules) result))
    delivery result = case result of
      Sent -> Right ()
      Uncertain -> Left (Failure "pin_saved_sms_uncertain" True)
      _ -> Left (failure "pin_saved_sms_failed")
