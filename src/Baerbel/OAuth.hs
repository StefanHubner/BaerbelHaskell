{-# LANGUAGE OverloadedStrings #-}
module Baerbel.OAuth
  ( Credentials, OAuthRules(..), loadCredentials, validateCredentials
  , tokenSource, tokenSourceWithClock, refreshRequest, validateTokenValue
  ) where

import Baerbel.Error (Failure(..), Result, failure, ioResult)
import Baerbel.HTTP (Body(..), Request(..), Send)
import Baerbel.JSON (decodeBytes, decodeValue, jsonOptions)
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Data.Aeson (FromJSON(..), Value(..), genericParseJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.IO (IOMode(ReadMode), withBinaryFile, hFileSize, Handle)
import Text.Read (readMaybe)

-- Secret-bearing records deliberately have no Show instance.
data Credentials = Credentials
  { clientId :: Text, clientSecret :: Text, refreshToken :: Text
  , grantedScopes :: Set Text, credentialEndpoint :: Maybe Text, credentialType :: Text }

data OAuthRules = OAuthRules
  { oauthEndpoint :: Text, oauthCredentialBytes :: Int, oauthFieldChars :: Int
  , oauthTokenChars :: Int, oauthResponseBytes :: Int, oauthTimeoutSeconds :: Int
  , oauthCacheSeconds :: Int, oauthRefreshSkewSeconds :: Int }
  deriving (Eq, Show, Generic)
instance FromJSON OAuthRules where parseJSON = genericParseJSON (jsonOptions 5)

newtype Scopes = Scopes (Set Text)
instance FromJSON Scopes where
  parseJSON value = Scopes . Set.fromList <$> (list <|> wordsOf)
    where
      list :: Parser [Text]
      list = parseJSON value
      wordsOf :: Parser [Text]
      wordsOf = Text.words <$> parseJSON value

instance FromJSON Credentials where
  parseJSON = withObject "authorized credentials" credentialFields

credentialFields :: KM.KeyMap Value -> Parser Credentials
credentialFields document = do
  ident <- document .: "client_id"
  secret <- document .: "client_secret"
  refreshValue <- document .: "refresh_token"
  Scopes scopes <- document .: "scopes"
  endpoint <- traverse parseJSON (KM.lookup "token_uri" document)
  kind <- maybe (pure "authorized_user") parseJSON (KM.lookup "type" document)
  pure (Credentials ident secret refreshValue scopes endpoint kind)

type Cached = Maybe (Text, Double)

loadCredentials :: OAuthRules -> Set Text -> Set Text -> FilePath -> IO (Result Credentials)
loadCredentials rules required allowed path = do
  raw <- ioResult "credentials_unavailable" (withBinaryFile path ReadMode readBounded)
  pure (join raw >>= decodeBytes "invalid_credentials" >>= validateCredentials rules required allowed)
  where
    readBounded :: Handle -> IO (Result BS.ByteString)
    readBounded handle = do
      size <- hFileSize handle
      either (pure . Left) (const (readBytes handle)) (credentialSize rules size)
    readBytes :: Handle -> IO (Result BS.ByteString)
    readBytes handle = do
      let limit = oauthCredentialBytes rules + 1
      bytes <- BS.hGet handle limit
      let size = fromIntegral (BS.length bytes)
      pure (bytes <$ credentialSize rules size)

credentialSize :: OAuthRules -> Integer -> Result ()
credentialSize rules size
  | size > fromIntegral (oauthCredentialBytes rules) = Left (failure "invalid_credentials")
  | otherwise = Right ()

validateCredentials :: OAuthRules -> Set Text -> Set Text -> Value -> Result Credentials
validateCredentials rules required allowed value = decodeValue "invalid_credentials" value >>= validate
  where
    validate :: Credentials -> Result Credentials
    validate credentials
      | Set.null required || not (required `Set.isSubsetOf` allowed) = Left (failure "invalid_required_scopes")
      | not (required `Set.isSubsetOf` grantedScopes credentials && grantedScopes credentials `Set.isSubsetOf` allowed) = Left (failure "invalid_scopes")
      | credentialType credentials /= "authorized_user" = Left (failure "invalid_credentials")
      | maybe False (/= oauthEndpoint rules) (credentialEndpoint credentials) = Left (failure "invalid_credentials")
      | any invalid [clientId credentials, clientSecret credentials, refreshToken credentials] = Left (failure "invalid_credentials")
      | otherwise = Right credentials
    invalid :: Text -> Bool
    invalid field = Text.null field || Text.length field > oauthFieldChars rules

tokenSource :: Send -> OAuthRules -> Set Text -> Set Text -> Credentials -> IO (IO (Result Text))
tokenSource sender rules required allowed credentials = tokenSourceWithClock sender rules required allowed credentials monotonicSeconds

monotonicSeconds :: IO Double
monotonicSeconds = (\value -> fromIntegral value / 1000000000) <$> getMonotonicTimeNSec

tokenSourceWithClock :: Send -> OAuthRules -> Set Text -> Set Text -> Credentials -> IO Double -> IO (IO (Result Text))
tokenSourceWithClock sender rules required allowed credentials clock = do
  cache <- newMVar Nothing
  pure (modifyMVar cache refreshCached)
  where
    refreshCached :: Cached -> IO (Cached, Result Text)
    refreshCached cached = do
      now <- clock
      maybe (refresh sender rules required allowed credentials clock cached) (\value -> pure (cached, Right value)) (cachedToken now cached)

cachedToken :: Double -> Cached -> Maybe Text
cachedToken now (Just (value, expiry)) | now < expiry = Just value
cachedToken _ _ = Nothing

refresh :: Send -> OAuthRules -> Set Text -> Set Text -> Credentials -> IO Double -> Cached -> IO (Cached, Result Text)
refresh sender rules required allowed credentials clock cached = do
  let request = refreshRequest rules credentials
  response <- sender request
  now <- clock
  let result = response >>= validateTokenValue rules required allowed
  pure (storeToken now cached result)

storeToken :: Double -> Cached -> Result (Text, Int) -> (Cached, Result Text)
storeToken _ previous (Left problem) = (previous, Left problem {failureUncertain = False})
storeToken now _ (Right (value, lifetime)) = (Just (value, now + fromIntegral lifetime), Right value)

refreshRequest :: OAuthRules -> Credentials -> Request
refreshRequest rules credentials = Request "POST" (oauthEndpoint rules) [] [] body (oauthResponseBytes rules) (oauthTimeoutSeconds rules)
  where
    bytes :: Text -> BS.ByteString
    bytes = Text.encodeUtf8
    body :: Body
    body = FormBody [("client_id", bytes (clientId credentials)), ("client_secret", bytes (clientSecret credentials)),
      ("refresh_token", bytes (refreshToken credentials)), ("grant_type", "refresh_token")]

validateTokenValue :: OAuthRules -> Set Text -> Set Text -> Value -> Result (Text, Int)
validateTokenValue rules required allowed (Object fields) = do
  checkScopes required allowed (KM.lookup "scope" fields)
  value <- checkAccess rules (KM.lookup "access_token" fields)
  expiry <- tokenLifetime (oauthCacheSeconds rules) (KM.lookup "expires_in" fields)
  let seconds = fromInteger (min (toInteger (oauthCacheSeconds rules)) (max 0 (toInteger expiry - toInteger (oauthRefreshSkewSeconds rules))))
  pure (value, seconds)
validateTokenValue _ _ _ _ = Left (failure "invalid_response")

checkScopes :: Set Text -> Set Text -> Maybe Value -> Result ()
checkScopes _ _ Nothing = Right ()
checkScopes required allowed (Just (String value))
  | required `Set.isSubsetOf` scopes && scopes `Set.isSubsetOf` allowed = Right ()
  where scopes = Set.fromList (Text.words value)
checkScopes _ _ _ = Left (failure "refresh_scopes_rejected")

checkAccess :: OAuthRules -> Maybe Value -> Result Text
checkAccess rules (Just (String value))
  | not (Text.null value) && Text.length value <= oauthTokenChars rules && not (Text.any invalid value) = Right value
  where
    invalid :: Char -> Bool
    invalid char = ord char < 33 || ord char > 126
checkAccess _ _ = Left (failure "invalid_access_token")

tokenLifetime :: Int -> Maybe Value -> Result Int
tokenLifetime fallback Nothing = Right fallback
tokenLifetime _ (Just (Number value)) = Right (boundedInteger (truncate value))
tokenLifetime _ (Just (Bool True)) = Right 1
tokenLifetime _ (Just (Bool False)) = Right 0
tokenLifetime _ (Just (String value)) = maybe invalid (Right . boundedInteger) parsed
  where
    raw :: Text
    raw = Text.strip value
    parsed :: Maybe Integer
    parsed = readMaybe (Text.unpack (fromMaybePlus raw))
    invalid :: Result Int
    invalid = Left (failure "invalid_token_expiry")
tokenLifetime _ _ = Left (failure "invalid_token_expiry")

fromMaybePlus :: Text -> Text
fromMaybePlus value = fromMaybe value (Text.stripPrefix "+" value)

boundedInteger :: Integer -> Int
boundedInteger value = fromInteger (min (toInteger (maxBound :: Int)) (max (toInteger (minBound :: Int)) value))
