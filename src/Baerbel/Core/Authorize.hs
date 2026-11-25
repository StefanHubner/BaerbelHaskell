module Baerbel.Core.Authorize where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.HTTP ( Request(..), Response(..), Body(..) )
import Baerbel.Core.JSON ( decodeBytes, decodeValue, camelOptions )
import Crypto.Hash ( Digest, SHA256, hash )
import Data.Aeson
    ( Value(..), FromJSON(..), genericParseJSON, object, (.=) )
import Data.Bifunctor ( second )
import Data.ByteArray ( convert, constEq )
import Data.Map.Strict ( Map )
import Data.Text ( Text )
import Data.Time
    ( UTCTime, addUTCTime, formatTime, defaultTimeLocale )
import GHC.Generics ( Generic )
import Network.HTTP.Types
    ( status200, status400, status404, renderQuery, Status, Query )
import Text.Read ( readMaybe )
import qualified Data.Aeson.Key ( Key )
import qualified Data.ByteString as BS ( ByteString, null )
import qualified Data.ByteString.Base64.URL as Base64
    ( encodeUnpadded )
import qualified Data.Aeson.KeyMap as KM ( lookup, KeyMap )
import qualified Data.Map.Strict as Map
    ( elems, lookup, findWithDefault )
import qualified Data.Set as Set
    ( disjoint, fromList, isSubsetOf, null, unions, Set )
import qualified Data.Text as T
    ( null, strip, toLower, unwords, words, unpack )
import qualified Data.Text.Encoding as T
    ( decodeUtf8, decodeUtf8', encodeUtf8 )

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

codeRequest :: AuthorizationRules -> ScopeGroups -> DesktopClient -> [Text] -> Text -> Text -> Text -> Result Request
codeRequest rules groups client scopes code verifier redirect
  | not (supportedScopes groups scopes) = Left (failure "authorization_scopes_unsupported")
  | otherwise = Right (Request "POST" (authTokenEndpoint rules) [] [] body (authResponseBytes rules) (authExchangeSeconds rules))
  where
    body = FormBody (map (second T.encodeUtf8)
      [("client_id", desktopId client), ("client_secret", desktopSecret client), ("code", code),
       ("code_verifier", verifier), ("redirect_uri", redirect), ("grant_type", "authorization_code")])

exchangeCode :: AuthorizationRules -> DesktopClient -> [Text] -> UTCTime -> Result Response -> Result Value
exchangeCode rules client scopes now response = response >>= checkedResponse >>= tokenDocument rules client scopes now

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

consentResult :: Either () (Result Text) -> Result Text
consentResult (Left ()) = Left (failure "authorization_callback_stopped")
consentResult (Right received) = received

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
