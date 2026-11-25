module Baerbel.Runtime.Application (checkConfiguration, runApplication) where

import Baerbel.Runtime.Config
    ( Settings, setting, rule, booleanSetting )
import Baerbel.Runtime.Error ( Failure(..), Result, failure )
import Baerbel.Runtime.Events ( withEvents )
import Baerbel.Runtime.HTTP ( newRawSender )
import Baerbel.Runtime.IncomingSession
    ( IncomingServices(..), incomingSession )
import Baerbel.Runtime.Logging ( Report, withLogging )
import Baerbel.Runtime.ManagedMedia ( newRegistry )
import Baerbel.Runtime.Messages ( Messages, withMessages )
import Baerbel.Runtime.PhoneNumber ( validateNumber )
import Baerbel.Runtime.Pin ( parsePinHash )
import Baerbel.Runtime.Pipeline
    ( preparePipeline, pipelineSession )
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
import System.Directory ( findExecutable )
import System.Posix.Signals
    ( Handler(..), installHandler, sigINT, sigTERM )
import qualified Data.ByteString as Bytes ( replicate )
import qualified Baerbel.Runtime.Effects as Effects ( continue )
import qualified Data.Text as Text ( null, unpack )

checkConfiguration :: Settings -> IO (Result ())
checkConfiguration settings = do
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
  start output = do
    checked <- checkNative settings
    Effects.continue (const (withServices settings (serveConfigured output))) checked
  serveConfigured output services = withNotifications settings output (runConfigured settings services output)

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

runConfigured :: Settings -> Services -> Report -> Maybe Messages -> IO (Result ())
runConfigured settings services output messages = Effects.continue start configured
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
        owner = ownerSession settings services output ownerRegistry handoffs
    selected <- chooseOwner settings output owner
    Effects.continue (startServer settings output incoming) selected

chooseOwner :: Settings -> Report -> CallHandler -> IO (Result CallHandler)
chooseOwner settings output native
  | setting settings "PHONE_ASSISTANT_VOICE_MODE" == "pipeline" = fmap (fmap (`pipelineSession` output)) (preparePipeline settings)
  | otherwise = pure (Right native)

startServer :: Settings -> Report -> CallHandler -> CallHandler -> IO (Result ())
startServer settings output incoming owner = case serverConfiguration settings output incoming owner of
  Left err -> pure (Left err)
  Right config -> do
    output "server_started" []
    serve config `finally` output "server_stopped" []

serverConfiguration :: Settings -> Report -> CallHandler -> CallHandler -> Result ServerConfig
serverConfiguration settings output incoming owner = do
  native <- telephonyConfiguration settings
  host <- rule settings "runtime" "host"
  port <- rule settings "runtime" "owner_port"
  incomingPortValue <- enabledPort "INCOMING_SCREENING_ENABLED" "incoming_port"
  homePortValue <- enabledPort "HOME_PHONE_ENABLED" "home_port"
  limit <- rule settings "runtime" "max_connections"
  sockets <- rule settings "runtime" "socket"
  let home = homeRouteHandler native validateNumber
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
