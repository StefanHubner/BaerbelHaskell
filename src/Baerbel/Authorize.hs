{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Authorize
  ( AuthorizationRules(..), AuthorizationOptions(..), AuthorizationEffects(..), DesktopClient
  , ScopeGroups, Callback(..), selectedScopes, readClient, clientDocument, callbackResult, pkceChallenge
  , authorizationURL, exchangeCode, prepareTokenPath, authorize, parseOptions
  ) where

import Baerbel.Error (Result, failure, ioResult)
import Baerbel.HTTP (RawSend, Request(..), Response(..), Body(..))
import Baerbel.JSON (decodeBytes, encodeBytes, decodeValue, camelOptions)
import Baerbel.Storage (atomicWrite)
import Control.Monad (join, void, unless)
import Data.Bifunctor (second)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (bracket, bracketOnError, try, IOException)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value(..), FromJSON(..), genericParseJSON, object, (.=))
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (convert, constEq)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, addUTCTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Network.HTTP.Types (Query, Status, ResponseHeaders, status200, status400, status404, renderQuery, hContentType, hCacheControl)
import Network.Socket (Socket, Family(AF_INET), SocketType(Stream), SockAddr(..), defaultProtocol, socket, close, bind, listen, getSocketName, tupleToHostAddress)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (FileStatus, getSymbolicLinkStatus, isDirectory, isSymbolicLink, fileOwner, fileMode, accessModes, setFileMode)
import System.Posix.User (getEffectiveUserID)
import System.Posix.Types (UserID)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Data.Bits ((.&.))

type ScopeGroups = Map Text [Text]

data AuthorizationRules = AuthorizationRules
  { authEndpoint :: Text, authLegacyEndpoint :: Text, authTokenEndpoint :: Text
  , authDefaultPort :: Int, authDefaultTimeout :: Double, authMaximumExpiry :: Double
  , authDefaultExpiry :: Double, authResponseBytes :: Int, authExchangeSeconds :: Int
  , authCallbackFields :: Int, authHeaderBytes :: Int, authMessages :: Map Text Text
  } deriving (Eq, Show, Generic)
instance FromJSON AuthorizationRules where parseJSON = genericParseJSON (camelOptions 4)

data AuthorizationOptions = AuthorizationOptions
  { authClientFile :: FilePath, authTokenFile :: Maybe FilePath
  , authPort :: Int, authTimeout :: Double, authNoBrowser :: Bool
  , authGmail :: Bool, authCalendar :: Bool, authContacts :: Bool }
  deriving (Eq, Show)

data AuthorizationEffects = AuthorizationEffects
  { authorizationSend :: RawSend, announceAuthorization :: Text -> IO ()
  , openAuthorizationBrowser :: Text -> IO (), authorizationClock :: IO UTCTime }

data DesktopClient = DesktopClient {desktopId :: Text, desktopSecret :: Text}

data Callback = Callback
  { callbackStatus :: Status, callbackMessage :: Text, callbackDecision :: Maybe (Result Text) }
  deriving (Eq, Show)

selectedScopes :: ScopeGroups -> AuthorizationOptions -> Result [Text]
selectedScopes groups options
  | null scopes = Left (failure "authorization_select_integration")
  | otherwise = Right scopes
  where
    scopes :: [Text]
    scopes = concat [values | (enabled, name) <- choices, enabled, Just values <- [Map.lookup name groups]]
    choices :: [(Bool, Text)]
    choices = [(authGmail options, "gmail"), (authCalendar options, "calendar"), (authContacts options, "contacts")]

readClient :: AuthorizationRules -> FilePath -> IO (Result DesktopClient)
readClient rules path = do
  expanded <- expandPath path
  bytes <- ioResult "authorization_client_unreadable" (BS.readFile expanded)
  pure (bytes >>= decodeBytes "authorization_client_invalid" >>= clientDocument rules)

clientDocument :: AuthorizationRules -> Value -> Result DesktopClient
clientDocument rules (Object document) = case KM.lookup "installed" document of
  Just (Object fields) -> do
    endpoint <- optionalText "auth_uri" (authEndpoint rules) fields
    tokenEndpoint <- optionalText "token_uri" (authTokenEndpoint rules) fields
    ident <- requiredText "client_id" fields
    secret <- requiredText "client_secret" fields
    checkedClient rules endpoint tokenEndpoint (DesktopClient ident secret)
  _ -> Left (failure "authorization_desktop_client_required")
clientDocument _ _ = Left (failure "authorization_desktop_client_required")

checkedClient :: AuthorizationRules -> Text -> Text -> DesktopClient -> Result DesktopClient
checkedClient rules endpoint tokenEndpoint client
  | endpoint `elem` [authEndpoint rules, authLegacyEndpoint rules] && tokenEndpoint == authTokenEndpoint rules = Right client
  | otherwise = Left (failure "authorization_client_endpoints_invalid")

requiredText :: Data.Aeson.Key.Key -> KM.KeyMap Value -> Result Text
requiredText key fields = case KM.lookup key fields of
  Just (String value) | not (T.null (T.strip value)) -> Right value
  _ -> Left (failure "authorization_client_invalid")

optionalText :: Data.Aeson.Key.Key -> Text -> KM.KeyMap Value -> Result Text
optionalText key fallback fields = maybe (Right fallback) (decodeValue "authorization_client_invalid") (KM.lookup key fields)

pkceChallenge :: Text -> Text
pkceChallenge verifier = T.decodeUtf8 (Base64.encodeUnpadded (convert digest))
  where
    digest :: Digest SHA256
    digest = hash (T.encodeUtf8 verifier)

randomText :: Int -> IO Text
randomText count = T.decodeUtf8 . Base64.encodeUnpadded <$> (getRandomBytes count :: IO BS.ByteString)

authorizationURL :: AuthorizationRules -> DesktopClient -> [Text] -> Text -> Text -> Text -> Text
authorizationURL rules client scopes redirect state challenge = authEndpoint rules <> T.decodeUtf8 query
  where
    fields :: [(BS.ByteString, Maybe BS.ByteString)]
    fields = map (\(key, value) -> (key, Just (T.encodeUtf8 value)))
      [("client_id", desktopId client), ("redirect_uri", redirect), ("response_type", "code"),
       ("scope", T.unwords scopes), ("state", state), ("code_challenge", challenge),
       ("code_challenge_method", "S256"), ("access_type", "offline"), ("prompt", "consent"), ("include_granted_scopes", "false")]
    query :: BS.ByteString
    query = renderQuery True fields

callbackResult :: AuthorizationRules -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Query -> Callback
callbackResult rules expected method path query
  | method /= "GET" || path /= "/" = response status404 "notFound" Nothing
  | length query > authCallbackFields rules = response status400 "invalidResponse" Nothing
  | not stateMatches = response status400 "invalidState" Nothing
  | any ((== "error") . fst) query = response status400 "declined" (Just (Left (failure "authorization_declined")))
  | otherwise = case values "code" of
      [Just code] | not (BS.null code) -> case T.decodeUtf8' code of
        Right decoded -> response status200 "received" (Just (Right decoded))
        Left _ -> response status400 "missingCode" Nothing
      _ -> response status400 "missingCode" Nothing
  where
    values :: BS.ByteString -> [Maybe BS.ByteString]
    values key = [value | (name, value) <- query, name == key]
    stateMatches :: Bool
    stateMatches = case values "state" of
      [Just value] -> constEq value expected
      _ -> False
    response :: Status -> Text -> Maybe (Result Text) -> Callback
    response status key = Callback status (message rules key)

message :: AuthorizationRules -> Text -> Text
message rules key = Map.findWithDefault "" key (authMessages rules)

callbackApplication :: AuthorizationRules -> Text -> TMVar (Result Text) -> Wai.Application
callbackApplication rules state selected request respond = do
  let result = callbackResult rules (T.encodeUtf8 state) (Wai.requestMethod request) (Wai.rawPathInfo request) (Wai.queryString request)
  sent <- respond (Wai.responseLBS (callbackStatus result) headers (BL.fromStrict (T.encodeUtf8 (callbackMessage result))))
  maybe (pure ()) (void . atomically . tryPutTMVar selected) (callbackDecision result)
  pure sent
  where
    headers :: ResponseHeaders
    headers = [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, "no-store"), ("Referrer-Policy", "no-referrer")]

codeRequest :: AuthorizationRules -> ScopeGroups -> DesktopClient -> [Text] -> Text -> Text -> Text -> Result Request
codeRequest rules groups client scopes code verifier redirect
  | not (supportedScopes groups scopes) = Left (failure "authorization_scopes_unsupported")
  | otherwise = Right (Request "POST" (authTokenEndpoint rules) [] [] body (authResponseBytes rules) (authExchangeSeconds rules))
  where
    body = FormBody (map (second T.encodeUtf8)
      [("client_id", desktopId client), ("client_secret", desktopSecret client), ("code", code),
       ("code_verifier", verifier), ("redirect_uri", redirect), ("grant_type", "authorization_code")])

exchangeCode :: AuthorizationRules -> ScopeGroups -> DesktopClient -> [Text] -> Text -> Text -> Text -> AuthorizationEffects -> IO (Result Value)
exchangeCode rules groups client scopes code verifier redirect effects = case codeRequest rules groups client scopes code verifier redirect of
  Left issue -> pure (Left issue)
  Right request -> do
    response <- authorizationSend effects request
    now <- authorizationClock effects
    pure (response >>= checkedResponse >>= tokenDocument rules client scopes now)

checkedResponse :: Response -> Result Value
checkedResponse response
  | responseStatus response /= 200 = Left (failure "authorization_exchange_failed")
  | otherwise = decodeBytes "authorization_invalid_token_response" (responseBody response)

supportedScopes :: ScopeGroups -> [Text] -> Bool
supportedScopes groups scopes = not (Set.null requested) && requested == union && all complete intersecting
  where
    requested :: Set.Set Text
    requested = Set.fromList scopes
    intersecting :: [Set.Set Text]
    intersecting = filter (not . Set.disjoint requested) (map Set.fromList (Map.elems groups))
    union :: Set.Set Text
    union = Set.unions intersecting
    complete :: Set.Set Text -> Bool
    complete group = group `Set.isSubsetOf` requested

tokenDocument :: AuthorizationRules -> DesktopClient -> [Text] -> UTCTime -> Value -> Result Value
tokenDocument rules client scopes now (Object fields) = do
  granted <- tokenText "scope" fields
  kind <- tokenText "token_type" fields
  access <- tokenText "access_token" fields
  refresh <- tokenText "refresh_token" fields
  lifetime <- expiryValue rules (KM.lookup "expires_in" fields)
  validateGrant scopes granted kind
  Right (authorizedDocument rules client scopes now access refresh lifetime)
tokenDocument _ _ _ _ _ = Left (failure "authorization_invalid_token_response")

validateGrant :: [Text] -> Text -> Text -> Result ()
validateGrant scopes granted kind
  | Set.fromList (T.words granted) /= Set.fromList scopes = Left (failure "authorization_scopes_mismatch")
  | T.toLower kind /= "bearer" = Left (failure "authorization_token_type_invalid")
  | otherwise = Right ()

tokenText :: Data.Aeson.Key.Key -> KM.KeyMap Value -> Result Text
tokenText key fields = case KM.lookup key fields of
  Just (String value) | not (T.null value) -> Right value
  _ -> Left (failure "authorization_offline_access_required")

expiryValue :: AuthorizationRules -> Maybe Value -> Result Double
expiryValue rules value = converted >>= validate
  where
    converted :: Result Double
    converted = case value of
      Nothing -> Right (authDefaultExpiry rules)
      Just (Number number) -> Right (realToFrac number)
      Just (Bool True) -> Right 1
      Just (Bool False) -> Right 0
      Just (String text) -> maybe invalid Right (readMaybe (T.unpack (T.strip text)))
      _ -> invalid
    invalid :: Result Double
    invalid = Left (failure "authorization_token_expiry_invalid")
    validate :: Double -> Result Double
    validate seconds
      | isNaN seconds || isInfinite seconds || seconds <= 0 || seconds > authMaximumExpiry rules = invalid
      | otherwise = Right seconds

authorizedDocument :: AuthorizationRules -> DesktopClient -> [Text] -> UTCTime -> Text -> Text -> Double -> Value
authorizedDocument rules client scopes now access refresh lifetime = object
  ["type" .= ("authorized_user" :: Text), "client_id" .= desktopId client, "client_secret" .= desktopSecret client,
   "refresh_token" .= refresh, "token" .= access, "token_uri" .= authTokenEndpoint rules, "scopes" .= scopes,
   "expiry" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" expiry]
  where
    expiry :: UTCTime
    expiry = addUTCTime (realToFrac lifetime) now

prepareTokenPath :: FilePath -> IO (Result FilePath)
prepareTokenPath raw = do
  path <- expandPath raw
  result <- ioResult "authorization_token_directory_invalid" (prepare path)
  pure (join result)
  where
    prepare :: FilePath -> IO (Result FilePath)
    prepare path = do
      let parent = takeDirectory path
      exists <- doesDirectoryExist parent
      unless exists (createDirectoryIfMissing True parent >> setFileMode parent 0o700)
      inspectPath path parent

inspectPath :: FilePath -> FilePath -> IO (Result FilePath)
inspectPath path parent = do
  metadata <- getSymbolicLinkStatus parent
  owner <- getEffectiveUserID
  either (pure . Left) (const (inspectToken path)) (privateDirectory owner metadata)

privateDirectory :: UserID -> FileStatus -> Result ()
privateDirectory owner metadata
  | isDirectory metadata && fileOwner metadata == owner && fileMode metadata .&. accessModes == 0o700 = Right ()
  | otherwise = Left (failure "authorization_token_directory_invalid")

inspectToken :: FilePath -> IO (Result FilePath)
inspectToken path = do
  metadata <- try (getSymbolicLinkStatus path)
  pure (checkedTokenPath path metadata)

checkedTokenPath :: FilePath -> Either IOException FileStatus -> Result FilePath
checkedTokenPath path (Right info)
  | isSymbolicLink info = Left (failure "authorization_token_symlink")
  | otherwise = Right path
checkedTokenPath path (Left problem)
  | isDoesNotExistError problem = Right path
  | otherwise = Left (failure "authorization_token_unavailable")

expandPath :: FilePath -> IO FilePath
expandPath ('~':'/':rest) = (</> rest) <$> getHomeDirectory
expandPath path = pure path

authorize :: AuthorizationRules -> ScopeGroups -> AuthorizationOptions -> DesktopClient -> AuthorizationEffects -> IO (Result ())
authorize rules groups options client effects = case selectedScopes groups options of
  Left problem -> pure (Left problem)
  Right scopes -> do
    prepared <- prepareTokenPath (fromMaybe (defaultTokenPath options) (authTokenFile options))
    either (pure . Left) (startAuthorization rules groups options client effects scopes) prepared

startAuthorization :: AuthorizationRules -> ScopeGroups -> AuthorizationOptions -> DesktopClient -> AuthorizationEffects -> [Text] -> FilePath -> IO (Result ())
startAuthorization rules groups options client effects scopes tokenPath = do
  verifier <- randomText 64
  state <- randomText 32
  let run = consent rules groups options client effects scopes tokenPath verifier state
  opened <- ioResult "authorization_callback_unavailable" (bracket (openCallback (authPort options)) close run)
  pure (join opened)

openCallback :: Int -> IO Socket
openCallback port = bracketOnError (socket AF_INET Stream defaultProtocol) close configure
  where
    configure :: Socket -> IO Socket
    configure connection = do
      let address = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))
      bind connection address
      listen connection 5
      pure connection

consent :: AuthorizationRules -> ScopeGroups -> AuthorizationOptions -> DesktopClient -> AuthorizationEffects -> [Text] -> FilePath -> Text -> Text -> Socket -> IO (Result ())
consent rules groups options client effects scopes tokenPath verifier state connection = do
  address <- getSocketName connection
  case address of
    SockAddrInet port _ -> consentAt redirect
      where redirect = "http://127.0.0.1:" <> T.pack (show port) <> "/"
    _ -> pure (Left (failure "authorization_callback_unavailable"))
  where
    consentAt :: Text -> IO (Result ())
    consentAt redirect = do
      let challenge = pkceChallenge verifier
          url = authorizationURL rules client scopes redirect state challenge
      announceAuthorization effects (T.replace "{url}" url (message rules "openURL"))
      unless (authNoBrowser options) (openAuthorizationBrowser effects url)
      received <- awaitConsent rules options state connection
      either (pure . Left) (exchangeAndSave rules groups client scopes verifier redirect effects tokenPath) received

awaitConsent :: AuthorizationRules -> AuthorizationOptions -> Text -> Socket -> IO (Result Text)
awaitConsent rules options state connection = do
  selected <- newEmptyTMVarIO
  let settings = Warp.setMaxTotalHeaderLength (authHeaderBytes rules) (Warp.setTimeout 1 (Warp.setOnException (\_ _ -> pure ()) Warp.defaultSettings))
      server = Warp.runSettingsSocket settings connection (callbackApplication rules state selected)
      waitCode = atomically (takeTMVar selected)
      micros = floor (authTimeout options * 1000000)
  result <- timeout micros (race server waitCode)
  pure (consentResult result)

consentResult :: Maybe (Either () (Result Text)) -> Result Text
consentResult Nothing = Left (failure "authorization_timeout")
consentResult (Just (Left ())) = Left (failure "authorization_callback_stopped")
consentResult (Just (Right received)) = received

exchangeAndSave :: AuthorizationRules -> ScopeGroups -> DesktopClient -> [Text] -> Text -> Text -> AuthorizationEffects -> FilePath -> Text -> IO (Result ())
exchangeAndSave rules groups client scopes verifier redirect effects path code = do
  token <- exchangeCode rules groups client scopes code verifier redirect effects
  either (pure . Left) save token
  where
    save :: Value -> IO (Result ())
    save token = do
      checked <- prepareTokenPath path
      either (pure . Left) (const (persist token)) checked
    persist :: Value -> IO (Result ())
    persist token = do
      result <- atomicWrite path (encodeBytes token <> "\n")
      either (pure . Left) (const saved) result
    saved :: IO (Result ())
    saved = announceAuthorization effects (T.replace "{path}" (T.pack path) (message rules "saved")) >> pure (Right ())

defaultTokenPath :: AuthorizationOptions -> FilePath
defaultTokenPath options = "~/.config/phone-assistant/" ++ name ++ "-token.json"
  where
    name :: String
    name
      | authGmail options = "gmail"
      | authContacts options && not (authCalendar options) = "contacts"
      | authCalendar options && not (authContacts options) = "calendar"
      | otherwise = "google"

parseOptions :: AuthorizationRules -> [String] -> Result AuthorizationOptions
parseOptions rules arguments = parse initial arguments >>= validate
  where
    initial :: AuthorizationOptions
    initial = AuthorizationOptions "" Nothing (authDefaultPort rules) (authDefaultTimeout rules) False True False False
    validate :: AuthorizationOptions -> Result AuthorizationOptions
    validate options
      | null (authClientFile options) = Left (failure "authorization_client_argument_required")
      | authPort options < 0 || authPort options > 65535 = Left (failure "authorization_port_invalid")
      | isNaN (authTimeout options) || isInfinite (authTimeout options) || authTimeout options <= 0 = Left (failure "authorization_timeout_invalid")
      | otherwise = Right options
    parse :: AuthorizationOptions -> [String] -> Result AuthorizationOptions
    parse options [] = Right options
    parse options ("--client-secrets":path:rest) = parse options {authClientFile = path} rest
    parse options ("--token-file":path:rest) = parse options {authTokenFile = Just path} rest
    parse options ("--no-browser":rest) = parse options {authNoBrowser = True} rest
    parse options ("--calendar":rest) = parse options {authCalendar = True} rest
    parse options ("--contacts":rest) = parse options {authContacts = True} rest
    parse options ("--no-gmail":rest) = parse options {authGmail = False} rest
    parse options ("--port":value:rest) = maybe invalid (\port -> parse options {authPort = port} rest) (readMaybe value)
    parse options ("--timeout":value:rest) = maybe invalid (\seconds -> parse options {authTimeout = seconds} rest) (readMaybe value)
    parse _ _ = invalid
    invalid :: Result AuthorizationOptions
    invalid = Left (failure "authorization_arguments_invalid")
