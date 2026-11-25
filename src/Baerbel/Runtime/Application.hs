module Baerbel.Runtime.Application (checkConfiguration, runApplication, checkAccounts, runAccounts) where

import Baerbel.Core.Accounts (Subscriber(..))
import Baerbel.Core.Config (Settings)
import Baerbel.Runtime.Accounts (AccountIngress(..), accountIngress)
import Baerbel.Runtime.Config
    ( setting, rule, booleanSetting )
import Baerbel.Runtime.Error ( Failure(..), Result, failure )
import Baerbel.Runtime.Events ( withEvents )
import Baerbel.Runtime.HTTP ( newRawSender )
import Baerbel.Runtime.IncomingSession
    ( IncomingServices(..), incomingSession )
import Baerbel.Runtime.Logging ( Report, withLogging )
import Baerbel.Runtime.ManagedMedia ( newRegistry )
import Baerbel.Runtime.Messages ( Messages, withMessages )
import Baerbel.Core.PhoneNumber ( numberValidator )
import Baerbel.Core.Routing ( NumberValidator )
import Baerbel.Runtime.Pin ( parsePinHash )
import Baerbel.Runtime.SMS ( SMSCredentials(..), withSMS )
import Baerbel.Runtime.Server
    ( ServerConfig(..), CallHandler, serve )
import Baerbel.Runtime.Services
    ( Services(..), withServices, checkServices )
import Baerbel.Runtime.Session
    ( ownerSession, telephonyConfiguration )
import Baerbel.Runtime.Speech ( withResampler, resample )
import Baerbel.Runtime.Telephony
    ( newHandoffs,
      homeRouteHandler,
      dialTimeoutSeconds,
      telephonyRules )
import Baerbel.Runtime.VAD ( withVAD, isSpeech )
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, newEmptyTMVarIO, takeTMVar, tryPutTMVar )
import Control.Exception ( bracket, finally )
import Control.Monad ( void )
import Data.Aeson ( (.=) )
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory ( findExecutable )
import System.Posix.Signals
    ( Handler(..), installHandler, sigINT, sigTERM )
import qualified Data.ByteString as Bytes ( replicate )
import qualified Baerbel.Runtime.Effects as Effects ( continue, runAll )
import qualified Data.Text as Text ( null, unpack )

checkConfiguration :: Settings -> IO (Result ())
checkConfiguration settings = Effects.continue check (numberValidator settings)
  where
  check _ = do
    checked <- checkNative settings
    Effects.continue (const (checkServices settings)) checked

checkNative :: Settings -> IO (Result ())
checkNative settings = case pinValid of
  Left err -> pure (Left err)
  Right () -> withVAD 2 (\vad -> isSpeech vad 8000 silence >>= checkResampling)
  where
  pin = setting settings "PHONE_ASSISTANT_PRIVATE_PIN_HASH"
  pinValid
    | Text.null pin = Right ()
    | otherwise = void (parsePinHash pin)
  silence = Bytes.replicate 320 0
  checkResampling = Effects.continue (const (withResampler 8000 24000 converted))
  converted resampler = do
    result <- resample resampler silence True
    Effects.continue (const (checkEncoder settings)) result

checkEncoder :: Settings -> IO (Result ())
checkEncoder settings
  | booleanSetting settings "PHONE_ASSISTANT_OWNER_NOTES" /= Right True = pure (Right ())
  | otherwise = do
      found <- findExecutable "ffmpeg"
      pure (maybe (Left (failure "note_encoder_unavailable")) (const (Right ())) found)

runApplication :: Settings -> IO (Result ())
runApplication settings = withLogging logPath (withShutdown . start)
  where
  configuredPath = setting settings "PHONE_ASSISTANT_LOG_FILE"
  logPath
    | Text.null configuredPath = Nothing
    | otherwise = Just (Text.unpack configuredPath)
  start output = Effects.continue (startValidated output) (numberValidator settings)
  startValidated output validate = do
    checked <- checkNative settings
    Effects.continue (const (withServices settings (serveConfigured output validate))) checked
  serveConfigured output validate services = withNotifications settings output (runConfigured settings validate services output)

withNotifications :: Settings -> Report -> (Maybe Messages -> IO (Result a)) -> IO (Result a)
withNotifications settings output action
  | booleanSetting settings "INCOMING_SMS_ENABLED" /= Right True = action Nothing
  | otherwise = Effects.continue open configured
  where
  configured = (,,) <$> rule settings "sms" "transport" <*> rule settings "incoming_messages" "service" <*> rule settings "incoming_events" "reader"
  credentials = SMSCredentials (setting settings "PHONE_ASSISTANT_SMS_USERNAME") (setting settings "PHONE_ASSISTANT_SMS_PASSWORD") (setting settings "OWNER_MOBILE_E164")
  path key = Text.unpack (setting settings key)
  open (smsRules, messageRules, eventRules) = do
    sender <- newRawSender
    let events messages = withEvents eventRules output messages (path "INCOMING_EVENTS_FILE") (path "INCOMING_EVENTS_STATE_FILE") (action (Just messages))
        messageService sms = withMessages messageRules output sms events
    withSMS smsRules credentials sender (path "PHONE_ASSISTANT_SMS_STATE_FILE") messageService

runConfigured :: Settings -> NumberValidator -> Services -> Report -> Maybe Messages -> IO (Result ())
runConfigured settings validate services output messages =
  withHandlers settings validate services output messages (startServer settings validate output)

withHandlers :: Settings -> NumberValidator -> Services -> Report -> Maybe Messages -> (CallHandler -> CallHandler -> IO (Result ())) -> IO (Result ())
withHandlers settings validate services output messages action = Effects.continue start configured
  where
  configured = (,,) <$> telephonyConfiguration settings <*> rule settings "runtime" "native_pause_extra_seconds" <*> rule settings "runtime" "socket"
  start (native, extra, sockets) = do
    let pauseMicros = (dialTimeoutSeconds native + extra) * 1000000
    ownerRegistry <- newRegistry sockets pauseMicros
    publicRegistry <- newRegistry sockets pauseMicros
    handoffs <- newHandoffs (telephonyRules native)
    bind ownerRegistry publicRegistry handoffs
  bind ownerRegistry publicRegistry handoffs = do
    let public = IncomingServices (lookupPublicCaller services) messages
        incoming = incomingSession settings public output publicRegistry handoffs
        owner = ownerSession settings validate services output ownerRegistry handoffs
    action incoming owner

checkAccounts :: Map Text Subscriber -> IO (Result ())
checkAccounts = Effects.runAll (checkConfiguration . subscriberSettings) . Map.elems

data AccountCalls = AccountCalls {ownerCall :: CallHandler, publicCall :: Maybe CallHandler}

runAccounts :: Settings -> Map Text Subscriber -> IO (Result ())
runAccounts shared accounts = withLogging logPath (withShutdown . start)
  where
    path = setting shared "PHONE_ASSISTANT_LOG_FILE"
    logPath | Text.null path = Nothing
            | otherwise = Just (Text.unpack path)
    start output = Effects.continue (open output) (numberValidator shared)
    open output validate = Effects.continue run (accountIngress shared accounts validate)
      where
      run entry = withAccounts validate output (Map.toList accounts) Map.empty (startAccounts shared validate output entry)

withAccounts :: NumberValidator -> Report -> [(Text, Subscriber)] -> Map Text AccountCalls -> (Map Text AccountCalls -> IO (Result ())) -> IO (Result ())
withAccounts _ _ [] prepared action = action prepared
withAccounts validate output ((ident, sub) : rest) prepared action =
  withServices settings withNotificationsFor
  where
    settings = subscriberSettings sub
    withNotificationsFor services = withNotifications settings output (handlers services)
    handlers services messages = withHandlers settings validate services output messages register
    register incoming owner = withAccounts validate output rest updated action
      where
      calls = AccountCalls owner (publicHandler incoming)
      updated = Map.insert ident calls prepared
    publicHandler incoming
      | booleanSetting settings "INCOMING_SCREENING_ENABLED" == Right True = Just incoming
      | otherwise = Nothing

startAccounts :: Settings -> NumberValidator -> Report -> AccountIngress -> Map Text AccountCalls -> IO (Result ())
startAccounts shared validate output ingress accounts = Effects.continue (serveReported output) configured
  where
    owner = dispatchAccount ingress True (Just . ownerCall) accounts
    incoming = dispatchAccount ingress False publicCall accounts
    configured = do
      cfg <- serverConfiguration shared validate output incoming owner
      screening <- rule shared "runtime" "incoming_port"
      agi <- rule shared "runtime" "home_port"
      let public = any hasPublic (Map.elems accounts)
          port | public = Just screening
               | otherwise = Nothing
      pure cfg {incomingPort = port, homePort = Just agi, homeHandler = accountAGI ingress}
    hasPublic = isJust . publicCall

dispatchAccount :: AccountIngress -> Bool -> (AccountCalls -> Maybe CallHandler) -> Map Text AccountCalls -> CallHandler
dispatchAccount ingress owner select accounts ident socket = do
  account <- identifyAccount ingress owner ident
  Effects.continue run (account >>= handler)
  where
    handler key = maybe rejected Right (Map.lookup key accounts >>= select)
    rejected = Left (failure "account_call_not_permitted")
    run call = call ident socket

startServer :: Settings -> NumberValidator -> Report -> CallHandler -> CallHandler -> IO (Result ())
startServer settings validate output incoming owner = case serverConfiguration settings validate output incoming owner of
  Left err -> pure (Left err)
  Right config -> serveReported output config

serveReported :: Report -> ServerConfig -> IO (Result ())
serveReported output config = do
  output "server_started" []
  serve config `finally` output "server_stopped" []

serverConfiguration :: Settings -> NumberValidator -> Report -> CallHandler -> CallHandler -> Result ServerConfig
serverConfiguration settings validate output incoming owner = do
  native <- telephonyConfiguration settings
  host <- rule settings "runtime" "host"
  port <- rule settings "runtime" "owner_port"
  incomingPortValue <- enabledPort "INCOMING_SCREENING_ENABLED" "incoming_port"
  homePortValue <- enabledPort "HOME_PHONE_ENABLED" "home_port"
  limit <- rule settings "runtime" "max_connections"
  sockets <- rule settings "runtime" "socket"
  let home = homeRouteHandler native validate
      report issue = output "call_failed" ["error" .= failureCode issue]
  pure (ServerConfig host port incomingPortValue homePortValue limit owner incoming home sockets report)
  where
  enabledPort flag key = do
    enabled <- booleanSetting settings flag
    optionalPort enabled key
  optionalPort False _ = Right Nothing
  optionalPort True key = Just <$> rule settings "runtime" key

withShutdown :: IO (Result ()) -> IO (Result ())
withShutdown action = do
  stopped <- newEmptyTMVarIO
  let handler = Catch (atomically (voidTry stopped))
      await = atomically (takeTMVar stopped) >> pure (Right ())
      run = fmap (either id id) (race action await)
  bracket (installHandler sigTERM handler Nothing) (\old -> installHandler sigTERM old Nothing) $ \_ ->
    bracket (installHandler sigINT handler Nothing) (\old -> installHandler sigINT old Nothing) (const run)
  where voidTry value = void (tryPutTMVar value ())
