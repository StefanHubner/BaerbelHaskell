{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Authorize
  ( module Core, AuthorizationEffects(..), readClient, authorize, prepareTokenPath ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( decodeBytes, encodeBytes )
import Baerbel.Runtime.Effects
    ( attempt, continue, mapResult, within )
import Baerbel.Runtime.Error ( ioResult )
import Baerbel.Runtime.HTTP ( Request, RawSend )
import Baerbel.Runtime.Storage ( atomicWrite )
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, newEmptyTMVarIO, takeTMVar, tryPutTMVar, TMVar )
import Control.Exception
    ( bracket, bracketOnError, try, IOException )
import Control.Monad ( (>=>), void, unless )
import Crypto.Random ( getRandomBytes )
import Data.Aeson ( Value )
import Data.Bits ( (.&.) )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Network.HTTP.Types
    ( hCacheControl, hContentType, ResponseHeaders )
import Network.Socket
    ( Socket,
      Family(AF_INET),
      SocketType(Stream),
      SockAddr(..),
      defaultProtocol,
      socket,
      close,
      bind,
      listen,
      getSocketName,
      tupleToHostAddress )
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory )
import System.FilePath ( takeDirectory, (</>) )
import System.IO.Error ( isDoesNotExistError )
import System.Posix.Files
    ( FileStatus,
      getSymbolicLinkStatus,
      isDirectory,
      isSymbolicLink,
      fileOwner,
      fileMode,
      accessModes,
      setFileMode )
import System.Posix.Types ( UserID )
import System.Posix.User ( getEffectiveUserID )
import Baerbel.Core.Authorize as Core
    ( Callback(..),
      DesktopClient(..),
      AuthorizationOptions(..),
      AuthorizationRules(..),
      ScopeGroups,
      selectedScopes,
      clientDocument,
      checkedClient,
      requiredText,
      optionalText,
      pkceChallenge,
      authorizationURL,
      callbackResult,
      message,
      codeRequest,
      exchangeCode,
      checkedResponse,
      supportedScopes,
      tokenDocument,
      validateGrant,
      tokenText,
      expiryValue,
      authorizedDocument,
      consentResult,
      defaultTokenPath,
      parseOptions )
import qualified Data.ByteString.Lazy as BL ( fromStrict )
import qualified Data.ByteString as BS ( ByteString, readFile )
import qualified Data.ByteString.Base64.URL as Base64
    ( encodeUnpadded )
import qualified Data.Text as T ( replace, pack )
import qualified Data.Text.Encoding as T ( decodeUtf8, encodeUtf8 )
import qualified Network.Wai as Wai
    ( responseLBS,
      Application,
      Request(queryString, requestMethod, rawPathInfo) )
import qualified Network.Wai.Handler.Warp as Warp
    ( setMaxTotalHeaderLength,
      setOnException,
      setTimeout,
      runSettingsSocket,
      defaultSettings )

data AuthorizationEffects = AuthorizationEffects
  { authorizationSend :: RawSend, announceAuthorization :: Text -> IO ()
  , openAuthorizationBrowser :: Text -> IO (), authorizationClock :: IO UTCTime }

readClient :: AuthorizationRules -> FilePath -> IO (Result DesktopClient)
readClient rules path = do
  expanded <- expandPath path
  let readBytes = ioResult "authorization_client_unreadable" (BS.readFile expanded)
      decode = decodeBytes "authorization_client_invalid" >=> clientDocument rules
  mapResult decode readBytes

randomText :: Int -> IO Text
randomText count = T.decodeUtf8 . Base64.encodeUnpadded <$> (getRandomBytes count :: IO BS.ByteString)

callbackApplication :: AuthorizationRules -> Text -> TMVar (Result Text) -> Wai.Application
callbackApplication rules state selected request respond = do
  let result = callbackResult rules (T.encodeUtf8 state) (Wai.requestMethod request) (Wai.rawPathInfo request) (Wai.queryString request)
  sent <- respond (Wai.responseLBS (callbackStatus result) headers (BL.fromStrict (T.encodeUtf8 (callbackMessage result))))
  maybe (pure ()) (void . atomically . tryPutTMVar selected) (callbackDecision result)
  pure sent
  where
    headers :: ResponseHeaders
    headers = [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, "no-store"), ("Referrer-Policy", "no-referrer")]

prepareTokenPath :: FilePath -> IO (Result FilePath)
prepareTokenPath raw = do
  path <- expandPath raw
  attempt "authorization_token_directory_invalid" (prepare path)
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
  continue (const (inspectToken path)) (privateDirectory owner metadata)

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
authorize rules groups options client effects = continue prepare (selectedScopes groups options)
  where
    prepare :: [Text] -> IO (Result ())
    prepare scopes = do
      let path = fromMaybe (defaultTokenPath options) (authTokenFile options)
      prepared <- prepareTokenPath path
      continue (startAuthorization rules groups options client effects scopes) prepared

startAuthorization :: AuthorizationRules -> ScopeGroups -> AuthorizationOptions -> DesktopClient -> AuthorizationEffects -> [Text] -> FilePath -> IO (Result ())
startAuthorization rules groups options client effects scopes tokenPath = do
  verifier <- randomText 64
  state <- randomText 32
  let run = consent rules groups options client effects scopes tokenPath verifier state
  attempt "authorization_callback_unavailable" (bracket (openCallback (authPort options)) close run)

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
      continue (exchangeAndSave rules groups client scopes verifier redirect effects tokenPath) received

awaitConsent :: AuthorizationRules -> AuthorizationOptions -> Text -> Socket -> IO (Result Text)
awaitConsent rules options state connection = do
  selected <- newEmptyTMVarIO
  let settings = Warp.setMaxTotalHeaderLength (authHeaderBytes rules) (Warp.setTimeout 1 (Warp.setOnException (\_ _ -> pure ()) Warp.defaultSettings))
      server = Warp.runSettingsSocket settings connection (callbackApplication rules state selected)
      waitCode = atomically (takeTMVar selected)
      micros = floor (authTimeout options * 1000000)
      deadline = failure "authorization_timeout"
  within deadline micros (consentResult <$> race server waitCode)

exchangeAndSave :: AuthorizationRules -> ScopeGroups -> DesktopClient -> [Text] -> Text -> Text -> AuthorizationEffects -> FilePath -> Text -> IO (Result ())
exchangeAndSave rules groups client scopes verifier redirect effects path code = continue exchange prepared
  where
    prepared = codeRequest rules groups client scopes code verifier redirect
    exchange :: Request -> IO (Result ())
    exchange request = do
      response <- authorizationSend effects request
      now <- authorizationClock effects
      continue save (exchangeCode rules client scopes now response)
    save :: Value -> IO (Result ())
    save token = do
      checked <- prepareTokenPath path
      continue (const (persist token)) checked
    persist :: Value -> IO (Result ())
    persist token = do
      result <- atomicWrite path (encodeBytes token <> "\n")
      continue (const saved) result
    saved :: IO (Result ())
    saved = announceAuthorization effects (T.replace "{path}" (T.pack path) (message rules "saved")) >> pure (Right ())
