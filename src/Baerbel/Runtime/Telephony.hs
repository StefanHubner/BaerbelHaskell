{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Baerbel.Runtime.Telephony
  ( TelephonyConfig(..), TelephonyRules(..), TelephonyMedia(..), OwnerControl(..), IncomingControl(..), IncomingEnd(..)
  , Handoffs, newHandoffs, claimHandoff, openOwner, openIncoming, homeRouteHandler, agiSession, routeHome, agiCommand, readAgiLine
  ) where

import Baerbel.Core.CallState
    ( Effect,
      CallState(callChannel, callResults, callId, callControlLost,
                callPhase, callRequest),
      DialRequest(dialRequestId, DialRequest, dialRoute, dialLabel),
      Phase(Conversing, Announcing, Connected, RedirectPending,
            Forwarded, Ended),
      initialCall,
      proposeRoute,
      requestDial,
      nativeEvent,
      requestForward,
      forwardResult )
import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Core.Incoming
    ( trustedEndpoint, callbackNumber, remainingBudget )
import Baerbel.Core.JSON ( resultValue )
import Baerbel.Core.Routing
    ( NumberValidator, Route, Permission(Owner, Screening), selectRoute )
import Baerbel.Core.Telephony
import Baerbel.Runtime.AMI
    ( Channel(channelMessageOnly, channelUniqueId, channelName),
      Message,
      AMIRules,
      field,
      ownerChannel,
      incomingChannel,
      AMI(..),
      connectAMI )
import Baerbel.Runtime.Concurrent ( stateMVar, updateMVar )
import Baerbel.Runtime.Recording ( Archive, openRecording )
import Control.Concurrent
    ( MVar, newMVar, readMVar, modifyMVar, withMVar )
import Control.Concurrent.STM
    ( TChan,
      atomically,
      newBroadcastTChanIO,
      dupTChan,
      readTChan,
      writeTChan )
import Control.Exception ( finally, onException )
import Control.Monad ( when, unless, void )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except
    ( ExceptT(..), runExceptT, except )
import Crypto.Random ( getRandomBytes )
import Data.Aeson ( object, Value(String, Object), KeyValue((.=)) )
import Data.ByteArray.Encoding ( Base(Base16), convertToBase )
import Data.ByteString ( ByteString )
import Data.Maybe ( fromMaybe, isNothing )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8, decodeUtf8', encodeUtf8 )
import Network.Socket ( Socket )
import System.Timeout ( timeout )
import qualified Data.ByteString as B
import qualified Baerbel.Runtime.Effects as Effects
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import qualified Network.Socket.ByteString as N
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

data TelephonyMedia = TelephonyMedia
  { announceRoute :: Route -> Maybe Text -> IO (Result Bool)
  , pauseMedia :: IO (), resumeExisting :: IO ()
  , waitResumed :: Int -> IO (Result ()), keypadDigit :: ByteString -> IO () }

data OwnerControl = OwnerControl
  { executeDirect :: Text -> Value -> Text -> IO (Result Value), closeOwner :: IO (), ownerRecording :: IO (Result Archive)
  , waitOwnerFinished :: IO (Result ()), ownerIsEnded :: IO Bool, ownerAnswered :: IO Bool }

data IncomingControl = IncomingControl
  { incomingNativeChannel :: Channel, incomingMessagesEnabled :: Bool
  , incomingCallback :: Maybe Text, incomingSeconds :: Double
  , forwardIncoming :: Text -> IO (Result ()), terminateIncoming :: IncomingEnd -> IO (Result ())
  , closeIncoming :: IO () }

data OwnerRuntime = OwnerRuntime
  { ownerAMI :: AMI, ownerState :: MVar CallState, ownerEvents :: TChan ()
  , ownerLock :: MVar (), ownerConfig :: TelephonyConfig, ownerMedia :: TelephonyMedia
  , ownerReconnect :: IO (Result ()) }

freshToken :: IO Text
freshToken = render <$> (getRandomBytes 16 :: IO ByteString)
  where
    render :: ByteString -> Text
    render = decodeUtf8 . convertToBase Base16

bindOwner :: AMIRules -> AMI -> Text -> IO (Result Channel)
bindOwner rules ami identifier = Effects.mapResult (ownerChannel rules identifier) statuses
  where
    statuses :: IO (Result [Message])
    statuses = amiAction ami "Status" [("Variables", "PA_CALL_ID,PA_OWNER_AUTHORIZED,PA_PERMISSION")] (Just "StatusComplete")

openOwner :: TelephonyConfig -> NumberValidator -> TelephonyMedia -> Text -> IO (Result OwnerControl)
openOwner cfg validate media identifier = do
  state <- newMVar Nothing
  events <- newBroadcastTChanIO
  connected <- connectAMI (amiRules cfg) (amiUsername cfg) (amiPassword cfg) (observeOwner state events media)
  Effects.continue (finishOwner cfg validate media identifier state events) connected

finishOwner :: TelephonyConfig -> NumberValidator -> TelephonyMedia -> Text -> MVar (Maybe CallState) -> TChan () -> AMI -> IO (Result OwnerControl)
finishOwner cfg validate media identifier pending events ami = do
  bound <- bindOwner (amiRules cfg) ami identifier
  case bound of
    Left problem -> amiClose ami >> pure (Left problem)
    Right channel -> createOwner cfg validate media identifier pending events ami channel

createOwner :: TelephonyConfig -> NumberValidator -> TelephonyMedia -> Text -> MVar (Maybe CallState) -> TChan () -> AMI -> Channel -> IO (Result OwnerControl)
createOwner cfg validate media identifier pending events ami channel = do
  let initial = initialCall identifier Owner channel
  state <- newMVar initial
  lock <- newMVar ()
  updateMVar pending (const (Just initial))
  active <- newMVar ami
  let proxy = proxyAMI active
  let reconnect = reconnectOwner cfg pending events media active
  let runtime = OwnerRuntime proxy state events lock cfg media reconnect
  pure (Right (OwnerControl (executeOwner runtime pending validate) (shutdownOwner runtime pending) (recordOwner runtime) (waitFinished runtime pending) (ended pending) (maybe False ((== Connected) . callPhase) <$> readMVar pending)))

-- Callback state is independent of the command lock, so Hangup and DTMF can
-- arrive while an announcement or native dial is awaiting completion.
observeOwner :: MVar (Maybe CallState) -> TChan () -> TelephonyMedia -> Message -> IO ()
observeOwner state events media message = do
  snapshot <- stateMVar state update
  let digit = snapshot >>= sendDigit message
  maybe (pure ()) (keypadDigit media) digit
  atomically (writeTChan events ())
  where
    update :: Maybe CallState -> (Maybe CallState, Maybe CallState)
    update Nothing = (Nothing, Nothing)
    update (Just previous) = let (next, _) = nativeEvent message previous in (Just next, Just next)

ended :: MVar (Maybe CallState) -> IO Bool
ended state = maybe True ((== Ended) . callPhase) <$> readMVar state

currentState :: OwnerRuntime -> MVar (Maybe CallState) -> IO CallState
currentState runtime pending = do
  previous <- readMVar (ownerState runtime)
  fromMaybe previous <$> readMVar pending

saveState :: OwnerRuntime -> MVar (Maybe CallState) -> CallState -> IO ()
saveState runtime pending state = do
  updateMVar (ownerState runtime) (const state)
  updateMVar pending preserve
  where
    preserve :: Maybe CallState -> Maybe CallState
    preserve (Just previous) | callPhase previous `elem` [Connected, Ended] = Just previous {callResults = M.union (callResults state) (callResults previous)}
    preserve _ = Just state

executeOwner :: OwnerRuntime -> MVar (Maybe CallState) -> NumberValidator -> Text -> Value -> Text -> IO (Result Value)
executeOwner runtime pending validate name args requestId = withMVar (ownerLock runtime) operation
  where
    operation :: () -> IO (Result Value)
    operation () = runExceptT $ do
      (mode, parsed) <- except (parseDirect (telephonyRules (ownerConfig runtime)) name args)
      let routes = routeSettings (ownerConfig runtime)
      route <- except (selectRoute validate routes Owner (argumentNumber parsed))
      state <- lift (currentState runtime pending)
      maybe (executeBound runtime pending mode requestId parsed route state) except (cachedCommand (directEnabled (ownerConfig runtime)) requestId state)

runDirect :: OwnerRuntime -> MVar (Maybe CallState) -> DirectMode -> Text -> DirectArgs -> Route -> IO (Result Value)
runDirect runtime pending mode requestId args route = do
  token <- freshToken
  attempt <- freshToken
  now <- Effects.monotonicSeconds
  state <- currentState runtime pending
  let request = DialRequest requestId attempt (argumentToken args) route (argumentLabel args) (argumentConfirmed args) token now
  case mode of
    InspectRoute -> do
      let (updated, result) = proposeRoute now token route (argumentLabel args) state
      saveState runtime pending updated
      pure (Right result)
    DialNumber -> executeDial runtime pending request state

executeDial :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> CallState -> IO (Result Value)
executeDial runtime pending request state = case requestDial (directEnabled (ownerConfig runtime)) request state of
  Left problem -> pure (Left problem)
  Right (updated, _, Just result) -> saveState runtime pending updated >> pure (Right result)
  Right (updated, _, Nothing) -> saveState runtime pending updated >> announceDial runtime pending request

announceDial :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> IO (Result Value)
announceDial runtime pending request = do
  spoken <- announceRoute (ownerMedia runtime) (dialRoute request) (dialLabel request)
  case spoken of
    Left _ -> storeOutcome runtime pending request (object ["status" .= ("error" :: Text), "result" .= ("rejected" :: Text), "error" .= ("route_announcement_failed_no_call" :: Text)])
    Right False -> storeOutcome runtime pending request (object ["status" .= ("ok" :: Text), "result" .= ("cancelled" :: Text), "reason" .= ("owner_interrupted_before_dial" :: Text)])
    Right True -> dialOrRecover runtime pending request `onException` recoverOwner runtime pending

owned :: OwnerRuntime -> MVar (Maybe CallState) -> IO (Result Channel)
owned runtime pending = runExceptT $ do
  state <- lift (currentState runtime pending)
  when (callControlLost state) (ExceptT (ownerReconnect runtime))
  let channel = callChannel state
  mapM_ (checkVariable (ownerAMI runtime) channel) [("PA_CALL_ID", callId state), ("PA_OWNER_AUTHORIZED", "1"), ("PA_PERMISSION", "owner")]
  pure channel

checkVariable :: AMI -> Channel -> (Text, Text) -> ExceptT Failure IO ()
checkVariable ami channel (name, expected) = do
  actual <- ExceptT (getVariable ami channel name)
  unless (actual == expected) (except (Left (failure "owner_call_required")))

nativeDial :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> ExceptT Failure IO Value
nativeDial runtime pending request = do
  channel <- ExceptT (owned runtime pending)
  let variables = dialVariables (dialTimeoutSeconds (ownerConfig runtime)) request
  ExceptT (setVariables (ownerAMI runtime) channel variables)
  ExceptT (stateMVar pending markDialing)
  lift (pauseMedia (ownerMedia runtime))
  ExceptT (redirectOwner runtime pending channel)
  ExceptT (awaitDial runtime pending request)

setVariables :: AMI -> Channel -> [(Text, Text)] -> IO (Result ())
setVariables ami channel variables = runExceptT (mapM_ set variables)
  where
    set :: (Text, Text) -> ExceptT Failure IO ()
    set (key, value) = void (ExceptT (amiAction ami "Setvar" [("Channel", channelName channel), ("Variable", key), ("Value", value)] Nothing))

redirect :: AMI -> Channel -> Text -> IO (Result ())
redirect ami channel context = fmap void action
  where
    action :: IO (Result [Message])
    action = amiAction ami "Redirect" [("Channel", channelName channel), ("Context", context), ("Exten", "s"), ("Priority", "1")] Nothing

awaitDial :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> IO (Result Value)
awaitDial runtime pending request = do
  events <- atomically (dupTChan (ownerEvents runtime))
  let micros = (dialTimeoutSeconds (ownerConfig runtime) + nativeDialGraceSeconds (telephonyRules (ownerConfig runtime))) * 1000000
  result <- timeout micros (waitResult runtime pending events (dialRequestId request))
  case result of
    Nothing -> recoverOwner runtime pending >> pure (Right (resultValue "failed"))
    Just value -> completeDial runtime pending value

waitResult :: OwnerRuntime -> MVar (Maybe CallState) -> TChan () -> Text -> IO Value
waitResult runtime pending events identifier = do
  state <- currentState runtime pending
  case M.lookup identifier (callResults state) of
    Just value -> pure value
    Nothing | callPhase state == Ended -> pure (resultValue "cancelled")
    Nothing -> atomically (readTChan events) >> waitResult runtime pending events identifier

completeDial :: OwnerRuntime -> MVar (Maybe CallState) -> Value -> IO (Result Value)
completeDial runtime pending result = do
  state <- currentState runtime pending
  let complete | callPhase state `elem` [Connected, Ended] = pure (Right result)
               | otherwise = resumeAfterResult runtime pending result
  complete

recoverOwner :: OwnerRuntime -> MVar (Maybe CallState) -> IO ()
recoverOwner runtime pending = do
  state <- currentState runtime pending
  let ready = callPhase state `elem` [Connected, Ended, Announcing] || isNothing (callRequest state)
  unless ready (recoverPaused runtime pending)

recoverPaused :: OwnerRuntime -> MVar (Maybe CallState) -> IO ()
recoverPaused runtime pending = do
  result <- timeout (nativeRecoveryMicros (telephonyRules (ownerConfig runtime))) recovery
  unless (result == Just (Right ())) (abandonOwner runtime pending)
  where
    recovery :: IO (Result ())
    recovery = runExceptT $ do
      channel <- ExceptT (owned runtime pending)
      state <- lift (currentState runtime pending)
      unless (callPhase state `elem` [Connected, Ended]) (resume channel)
    resume :: Channel -> ExceptT Failure IO ()
    resume channel = do
      ExceptT (redirect (ownerAMI runtime) channel "phone-assistant-resume")
      ExceptT (waitResumed (ownerMedia runtime) (nativeRecoveryResumeSeconds (telephonyRules (ownerConfig runtime))))

abandonOwner :: OwnerRuntime -> MVar (Maybe CallState) -> IO ()
abandonOwner runtime pending = do
  ownership <- owned runtime pending
  either (const (pure ())) (\channel -> void (hangup (ownerAMI runtime) channel "16")) ownership
  updateMVar pending (fmap (\state -> state {callPhase = Ended}))
  atomically (writeTChan (ownerEvents runtime) ())

recordOwner :: OwnerRuntime -> IO (Result Archive)
recordOwner runtime = runExceptT $ do
  state <- lift (readMVar (ownerState runtime))
  result <- ExceptT (getVariable (ownerAMI runtime) (callChannel state) "PA_RECORDING_ACTIVE")
  unless (result == "1") (except (Left (failure "call_recording_unavailable")))
  ExceptT (openRecording directory (channelUniqueId (callChannel state)))
  where
    directory :: FilePath
    directory = recordingDirectory (ownerConfig runtime)

getVariable :: AMI -> Channel -> Text -> IO (Result Text)
getVariable ami channel name = fmap (fmap readValue) action
  where
    action :: IO (Result [Message])
    action = amiAction ami "Getvar" [("Channel", channelName channel), ("Variable", name)] Nothing
    readValue :: [Message] -> Text
    readValue [] = ""
    readValue (message : _) = field "Value" message

waitFinished :: OwnerRuntime -> MVar (Maybe CallState) -> IO (Result ())
waitFinished runtime pending = atomically (dupTChan (ownerEvents runtime)) >>= loop
  where
    loop :: TChan () -> IO (Result ())
    loop events = do
      state <- currentState runtime pending
      let proceed | callControlLost state = verify events
                  | otherwise = await events state
      proceed
    verify :: TChan () -> IO (Result ())
    verify events = do
      result <- owned runtime pending
      either finished (const (loop events)) result
    finished :: Failure -> IO (Result ())
    finished problem
      | failureCode problem == "owner_call_required" = pure (Right ())
      | otherwise = pure (Left problem)
    await :: TChan () -> CallState -> IO (Result ())
    await events state
      | callPhase state == Ended = pure (Right ())
      | otherwise = atomically (readTChan events) >> loop events

shutdownOwner :: OwnerRuntime -> MVar (Maybe CallState) -> IO ()
shutdownOwner runtime pending = do
  current <- owned runtime pending
  case current of
    Right channel -> void (hangup (ownerAMI runtime) channel "16")
    Left _ -> pure ()
  amiClose (ownerAMI runtime)

hangup :: AMI -> Channel -> Text -> IO (Result ())
hangup ami channel cause = fmap void action
  where
    action :: IO (Result [Message])
    action = amiAction ami "Hangup" [("Channel", channelName channel), ("Cause", cause)] Nothing

data Handoffs = Handoffs TelephonyRules (MVar (M.Map Text PendingHandoff))

newHandoffs :: TelephonyRules -> IO Handoffs
newHandoffs rules = Handoffs rules <$> newMVar M.empty

freshUUID :: IO Text
freshUUID = UUID.toText <$> UUID.nextRandom

prepareHandoff :: Handoffs -> Channel -> Text -> Text -> IO (Result Text)
prepareHandoff (Handoffs rules pending) channel identifier text = do
  now <- Effects.monotonicSeconds
  token <- freshUUID
  stateMVar pending (insert now token)
  where
    insert :: Double -> Text -> M.Map Text PendingHandoff -> (M.Map Text PendingHandoff, Result Text)
    insert now token records
      | M.size active >= nativeHandoffLimit rules = (active, Left (failure "incoming_handoff_capacity"))
      | T.null text || T.length text > nativeHandoffLineChars rules = (active, Left (failure "invalid_owner_handoff_line"))
      | otherwise = (M.insert token (PendingHandoff channel identifier text (now + nativeHandoffSeconds rules)) active, Right token)
      where
        active :: M.Map Text PendingHandoff
        active = M.filter ((> now) . handoffDeadline) records

forgetHandoff :: Handoffs -> Text -> IO ()
forgetHandoff (Handoffs _ pending) token = updateMVar pending (M.delete token)

claimHandoff :: TelephonyConfig -> Handoffs -> Text -> IO (Result (Maybe Text))
claimHandoff cfg (Handoffs _ pending) token = do
  now <- Effects.monotonicSeconds
  claimed <- stateMVar pending (consume now)
  maybe (pure (Right Nothing)) (verifyHandoff cfg token) claimed
  where
    consume :: Double -> M.Map Text PendingHandoff -> (M.Map Text PendingHandoff, Maybe PendingHandoff)
    consume now records = (M.delete token active, M.lookup token active)
      where
        active :: M.Map Text PendingHandoff
        active = M.filter ((> now) . handoffDeadline) records

verifyHandoff :: TelephonyConfig -> Text -> PendingHandoff -> IO (Result (Maybe Text))
verifyHandoff cfg token pending = do
  connected <- connectAMI (amiRules cfg) (amiUsername cfg) (amiPassword cfg) (const (pure ()))
  Effects.continue inspect connected
  where
    inspect :: AMI -> IO (Result (Maybe Text))
    inspect ami = runExceptT (checkHandoff ami token pending) `finally` amiClose ami

checkHandoff :: AMI -> Text -> PendingHandoff -> ExceptT Failure IO (Maybe Text)
checkHandoff ami token pending = do
  statuses <- ExceptT (amiAction ami "Status" [("Variables", "PA_CALL_ID,PA_PERMISSION,PA_OWNER_AUTHORIZED,PA_INCOMING_MODE,PA_HANDOFF_ID,PA_HANDOFF_PARENT")] (Just "StatusComplete"))
  channel <- except (handoffChannel token pending statuses)
  endpoint <- ExceptT (getVariable ami channel "CHANNEL(endpoint)")
  unless (ownerEndpoint channel endpoint) (except (Left (failure "incoming_handoff_untrusted_endpoint")))
  pure (Just (handoffText pending))

openIncoming :: TelephonyConfig -> Handoffs -> Text -> IO () -> IO (Result IncomingControl)
openIncoming cfg handoffs identifier pause = do
  connected <- connectAMI (amiRules cfg) (amiUsername cfg) (amiPassword cfg) (const (pure ()))
  Effects.continue bind connected
  where
    bind :: AMI -> IO (Result IncomingControl)
    bind ami = do
      result <- runExceptT (bindIncoming ami handoffs identifier pause)
      either (const (amiClose ami >> pure result)) (const (pure result)) result

incomingStatuses :: AMI -> IO (Result [Message])
incomingStatuses ami = amiAction ami "Status" [("Variables", "PA_CALL_ID,PA_PERMISSION,PA_OWNER_AUTHORIZED,PA_INCOMING_MODE")] (Just "StatusComplete")

bindIncoming :: AMI -> Handoffs -> Text -> IO () -> ExceptT Failure IO IncomingControl
bindIncoming ami handoffs identifier pause = do
  statuses <- ExceptT (incomingStatuses ami)
  channel <- except (incomingChannel identifier statuses)
  endpoint <- ExceptT (getVariable ami channel "CHANNEL(endpoint)")
  unless (trustedEndpoint channel endpoint) (except (Left (failure "incoming_untrusted_endpoint")))
  configureIncoming ami handoffs identifier pause channel

configureIncoming :: AMI -> Handoffs -> Text -> IO () -> Channel -> ExceptT Failure IO IncomingControl
configureIncoming ami handoffs identifier pause channel = do
  enabled <- ExceptT (getVariable ami channel "GLOBAL(PA_INCOMING_MESSAGES_ENABLED)")
  budget <- ExceptT (getVariable ami channel "TIMEOUT(absolute)") >>= except . remainingBudget
  privacy <- ExceptT (getVariable ami channel "CDR(tf_privacy)")
  callback <- ExceptT (getVariable ami channel "TF_CALLBACK")
  state <- lift (newMVar (initialCall identifier Screening channel))
  when (channelMessageOnly channel && enabled /= "1") (except (Left (failure "incoming_messages_disabled")))
  let messages = enabled == "1"
      number = callbackNumber privacy callback
  pure (makeIncoming ami handoffs identifier pause channel state messages budget number)

makeIncoming :: AMI -> Handoffs -> Text -> IO () -> Channel -> MVar CallState -> Bool -> Double -> Maybe Text -> IncomingControl
makeIncoming ami handoffs identifier pause channel state messages budget callback = IncomingControl channel messages callback budget forward terminate close
  where
    forward :: Text -> IO (Result ())
    forward = forwardNative ami handoffs identifier pause channel state
    terminate :: IncomingEnd -> IO (Result ())
    terminate = terminateNative ami identifier channel state
    close :: IO ()
    close = void (terminate ScreeningError) `finally` amiClose ami

isIncomingOwned :: AMI -> Text -> Channel -> IO (Result Bool)
isIncomingOwned ami identifier expected = fmap (fmap valid) (incomingStatuses ami)
  where
    valid :: [Message] -> Bool
    valid messages = incomingChannel identifier messages == Right expected

forwardNative :: AMI -> Handoffs -> Text -> IO () -> Channel -> MVar CallState -> Text -> IO (Result ())
forwardNative ami handoffs identifier pause channel state text = modifyMVar state proceed
  where
    proceed :: CallState -> IO (CallState, Result ())
    proceed previous = do
      ownership <- isIncomingOwned ami identifier channel
      let requested = ownership >>= (`requestForward` previous)
      either (\problem -> pure (previous, Left problem)) (submit previous) requested
    submit :: CallState -> (CallState, [Effect]) -> IO (CallState, Result ())
    submit _ (updated, []) = pure (updated, Right ())
    submit previous (updated, _) = do
      prepared <- prepareHandoff handoffs channel identifier text
      either (\problem -> pure (previous, Left problem)) (send previous updated) prepared
    send :: CallState -> CallState -> Text -> IO (CallState, Result ())
    send previous updated token = do
      installed <- setVariables ami channel [("PA_HANDOFF_ID", token)]
      either (\problem -> forgetHandoff handoffs token >> pure (previous, Left problem)) (const (redirectIncoming ami handoffs pause channel token updated)) installed

redirectIncoming :: AMI -> Handoffs -> IO () -> Channel -> Text -> CallState -> IO (CallState, Result ())
redirectIncoming ami handoffs pause channel token state = do
  pause
  result <- redirect ami channel "phone-assistant-screened-forward"
  when (either ((== "ami_action_rejected") . failureCode) (const False) result) (forgetHandoff handoffs token)
  pure (forwardResult result state, result)

terminateNative :: AMI -> Text -> Channel -> MVar CallState -> IncomingEnd -> IO (Result ())
terminateNative ami identifier channel state reason = modifyMVar state proceed
  where
    proceed :: CallState -> IO (CallState, Result ())
    proceed previous | callPhase previous `elem` [RedirectPending, Forwarded, Ended] = pure (previous, Right ())
    proceed previous = do
      ownership <- isIncomingOwned ami identifier channel
      let stop | ownership == Right True = terminate
               | otherwise = pure (Right ())
      result <- stop
      pure (previous {callPhase = Ended}, result)
    terminate :: IO (Result ())
    terminate = runExceptT (ExceptT (setVariables ami channel [("TF_RESULT", incomingReason reason)]) >> ExceptT (hangup ami channel "21"))

homeRouteHandler :: TelephonyConfig -> NumberValidator -> Socket -> IO (Result ())
homeRouteHandler cfg validate connection = agiSession rules (nativeAGIMicros rules) connection route
  where
    rules = telephonyRules cfg
    route = routeHome cfg validate connection

agiSession :: TelephonyRules -> Int -> Socket -> (M.Map Text Text -> ExceptT Failure IO ()) -> IO (Result ())
agiSession rules duration connection action = Effects.within (failure "agi_timeout") duration guarded
  where
    guarded = Effects.attempt "agi_connection_failed" (runExceptT receive)
    receive = ExceptT (readAgiHeaders rules connection 0 0 M.empty) >>= action

routeHome :: TelephonyConfig -> NumberValidator -> Socket -> M.Map Text Text -> ExceptT Failure IO ()
routeHome cfg validate connection headers = do
  number <- except (homeIdentity cfg headers)
  mapM_ (ExceptT . uncurry (agiCommand (telephonyRules cfg) connection)) homeChecks
  canonical <- except (validate number)
  route <- except (selectRoute Right (routeSettings cfg) Owner canonical)
  mapM_ (ExceptT . uncurry (agiCommand (telephonyRules cfg) connection)) (homeCommands route)

agiCommand :: TelephonyRules -> Socket -> Text -> Text -> IO (Result ())
agiCommand rules connection command expected = do
  N.sendAll connection (encodeUtf8 (command <> "\n"))
  Effects.mapResult verify (readAgiLine rules connection)
  where
    verify :: Text -> Result ()
    verify line
      | line == expected = Right ()
      | otherwise = Left (failure "agi_command_failed")

readAgiLine :: TelephonyRules -> Socket -> IO (Result Text)
readAgiLine rules connection = collect [] 0
  where
    collect :: [ByteString] -> Int -> IO (Result Text)
    collect parts size
      | size >= nativeAGILineBytes rules = pure (Left (failure "invalid_agi_line"))
      | otherwise = N.recv connection 1 >>= consume parts size
    consume :: [ByteString] -> Int -> ByteString -> IO (Result Text)
    consume parts size byte
      | B.null byte = pure (Left (failure "invalid_agi_line"))
      | byte == "\n" = pure (decodeLine (B.concat (reverse parts)))
      | otherwise = collect (byte : parts) (size + 1)
    decodeLine :: ByteString -> Result Text
    decodeLine bytes = either (const (Left (failure "invalid_agi_line"))) (Right . T.dropWhileEnd (== '\r')) (decodeUtf8' bytes)

readAgiHeaders :: TelephonyRules -> Socket -> Int -> Int -> M.Map Text Text -> IO (Result (M.Map Text Text))
readAgiHeaders rules connection count size headers
  | count >= nativeAGIHeaderCount rules || size > nativeAGIHeaderBytes rules = pure (Left (failure "agi_header_limit"))
  | otherwise = readAgiLine rules connection >>= consume
  where
    consume :: Result Text -> IO (Result (M.Map Text Text))
    consume (Left problem) = pure (Left problem)
    consume (Right "") = pure (Right headers)
    consume (Right line) = case header line of
      Left problem -> pure (Left problem)
      Right (key, value) -> readAgiHeaders rules connection (count + 1) (size + T.length line) (M.insert key value headers)
    header :: Text -> Result (Text, Text)
    header line
      | validKey key && not (T.null rest) && M.notMember key headers = Right (key, T.drop 2 rest)
      | otherwise = Left (failure "invalid_agi_headers")
      where (key, rest) = T.breakOn ": " line
    validKey :: Text -> Bool
    validKey key = T.isPrefixOf "agi_" key && T.length key > 4 && T.all (`elem` ("abcdefghijklmnopqrstuvwxyz_0123456789" :: String)) (T.drop 4 key)

storeOutcome :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> Value -> IO (Result Value)
storeOutcome runtime pending request result = do
  state <- currentState runtime pending
  let updated = completedOutcome (dialRequestId request) result state
  saveState runtime pending updated
  pure (Right result)

redirectOwner :: OwnerRuntime -> MVar (Maybe CallState) -> Channel -> IO (Result ())
redirectOwner runtime pending channel = do
  result <- redirect (ownerAMI runtime) channel "phone-assistant-direct-call"
  when (either ((== "ami_action_rejected") . failureCode) (const False) result) restore
  pure result
  where
    restore :: IO ()
    restore = do
      resumeExisting (ownerMedia runtime)
      state <- currentState runtime pending
      saveState runtime pending state {callPhase = Conversing, callRequest = Nothing}

dialOrRecover :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> IO (Result Value)
dialOrRecover runtime pending request = do
  result <- runExceptT (nativeDial runtime pending request)
  case result of
    Right value -> storeFinal runtime pending request (addDestination request value)
    Left _ -> do
      recoverOwner runtime pending
      state <- currentState runtime pending
      let output = failedDial request state
      storeOutcome runtime pending request output

executeBound :: OwnerRuntime -> MVar (Maybe CallState) -> DirectMode -> Text -> DirectArgs -> Route -> CallState -> ExceptT Failure IO Value
executeBound runtime pending mode identifier args route state = do
  channel <- ExceptT (bindOwner (amiRules (ownerConfig runtime)) (ownerAMI runtime) (callId state))
  except (verifyHome route channel)
  ExceptT (runDirect runtime pending mode identifier args route)

storeFinal :: OwnerRuntime -> MVar (Maybe CallState) -> DialRequest -> Value -> IO (Result Value)
storeFinal runtime pending request output = do
  state <- currentState runtime pending
  let results = M.insert (dialRequestId request) output (callResults state)
  saveState runtime pending state {callResults = results}
  pure (Right output)

proxyAMI :: MVar AMI -> AMI
proxyAMI active = AMI request close
  where
    request :: Text -> [(Text, Text)] -> Maybe Text -> IO (Result [Message])
    request name fields complete = withMVar active (\ami -> amiAction ami name fields complete)
    close :: IO ()
    close = readMVar active >>= amiClose

reconnectOwner :: TelephonyConfig -> MVar (Maybe CallState) -> TChan () -> TelephonyMedia -> MVar AMI -> IO (Result ())
reconnectOwner cfg state events media active = do
  connected <- connectAMI (amiRules cfg) (amiUsername cfg) (amiPassword cfg) (observeOwner state events media)
  case connected of
    Left problem -> pure (Left problem)
    Right next -> replaceAMI state active next

replaceAMI :: MVar (Maybe CallState) -> MVar AMI -> AMI -> IO (Result ())
replaceAMI state active next = do
  previous <- stateMVar active (next,)
  amiClose previous
  updateMVar state (fmap (\current -> current {callControlLost = False}))
  pure (Right ())

resumeAfterResult :: OwnerRuntime -> MVar (Maybe CallState) -> Value -> IO (Result Value)
resumeAfterResult runtime pending result
  | disconnected result = recoverOwner runtime pending >> pure (Right result)
  | otherwise = fmap (fmap (const result)) (waitResumed (ownerMedia runtime) (nativeResultResumeSeconds (telephonyRules (ownerConfig runtime))))
  where
    disconnected :: Value -> Bool
    disconnected (Object fields) = KM.lookup "reason" fields == Just (String "call_control_disconnected")
    disconnected _ = False
