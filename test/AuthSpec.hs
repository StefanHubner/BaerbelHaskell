{-# LANGUAGE OverloadedStrings #-}
module AuthSpec (spec) where

import Baerbel.Authorize
import Baerbel.Config (Global, User(..), resolveSettings, rule)
import Baerbel.Error (Result, failure)
import Baerbel.HTTP (Request(..), Response(..), Body(..), newRawSender)
import Baerbel.OAuth
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, async, wait)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_, modifyMVar)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Control.Exception (bracket)
import Data.Aeson (Value(..), object, (.=), eitherDecodeStrict', encode)
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime(..), fromGregorian)
import Network.HTTP.Types (status200, status400, parseQuery, renderQuery)
import System.Directory (getTemporaryDirectory, removeFile, createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Posix.Files (setFileMode, getFileStatus, fileMode, accessModes)
import Data.Bits ((.&.))
import Test.Hspec

spec :: Spec
spec = do
  describe "OAuth refresh boundary" refreshSpec
  describe "Desktop OAuth consent boundary" authorizationSpec

withAuthRules :: (OAuthRules -> AuthorizationRules -> ScopeGroups -> Expectation) -> Expectation
withAuthRules check = do
  bytes <- BS.readFile "config/global.json"
  case eitherDecodeStrict' bytes of
    Left problem -> expectationFailure problem
    Right global -> configure (global :: Global)
  where
    configure :: Global -> Expectation
    configure global = case rules global of
      Left problem -> expectationFailure (show problem)
      Right (refreshRules, authorizationRules, groups) -> check refreshRules authorizationRules groups
    rules :: Global -> Result (OAuthRules, AuthorizationRules, ScopeGroups)
    rules global = do
      let settings = resolveSettings global (User Map.empty)
      refreshRules <- rule settings "oauth" "refresh"
      authorizationRules <- rule settings "oauth" "authorization"
      gmail <- rule settings "oauth" "gmail"
      calendar <- rule settings "oauth" "calendar"
      contacts <- rule settings "oauth" "contacts"
      pure (refreshRules, authorizationRules, Map.fromList [("gmail", gmail), ("calendar", calendar), ("contacts", contacts)] )

credentialDocument :: [Text] -> Value
credentialDocument scopes = object ["client_id" .= ("client" :: Text), "client_secret" .= ("secret" :: Text),
  "refresh_token" .= ("refresh" :: Text), "scopes" .= scopes]

field :: Data.Aeson.Key.Key -> Value -> Value -> Value
field key value (Object fields) = Object (KM.insert key value fields)
field _ _ value = value

refreshSpec :: Spec
refreshSpec = do
  it "rejects a substituted credential endpoint, type, and oversized secret" $ withAuthRules $ \rules _ groups -> do
    let required = Set.fromList (Map.findWithDefault [] "gmail" groups)
        allowed = Set.fromList (concat (Map.elems groups))
        original = credentialDocument (Set.toList required)
        changed = [field "token_uri" (String "https://other.invalid/token") original,
                   field "type" (String "service_account") original,
                   field "token_uri" Null original,
                   field "client_secret" (String (T.replicate 8193 "a")) original]
    mapM_ (\document -> void (validateCredentials rules required allowed document) `shouldBe` Left (failure "invalid_credentials")) changed
    void (validateCredentials rules required allowed original) `shouldBe` Right ()
  it "preserves original expiry conversion and rejects bad tokens/scopes" $ withAuthRules $ \rules _ groups -> do
    let required = Set.fromList (Map.findWithDefault [] "gmail" groups)
        allowed = Set.fromList (concat (Map.elems groups))
        token = object ["access_token" .= ("token" :: Text), "expires_in" .= ("3600" :: Text)]
    validateTokenValue rules required allowed token `shouldBe` Right ("token", 3540)
    validateTokenValue rules required allowed (field "expires_in" (Number 3600.9) token) `shouldBe` Right ("token", 3540)
    validateTokenValue rules required allowed (field "expires_in" Null token) `shouldBe` Left (failure "invalid_token_expiry")
    validateTokenValue rules required allowed (field "access_token" (String "bad token") token) `shouldBe` Left (failure "invalid_access_token")
    validateTokenValue rules required allowed (field "scope" (String "other") token) `shouldBe` Left (failure "refresh_scopes_rejected")
  it "serializes concurrent refresh and expires against the monotonic clock" $ withAuthRules $ \rules _ groups -> do
    let required = Set.fromList (Map.findWithDefault [] "gmail" groups)
        allowed = Set.fromList (concat (Map.elems groups))
    case validateCredentials rules required allowed (credentialDocument (Set.toList required)) of
      Left problem -> expectationFailure (show problem)
      Right credentials -> checkCache rules required allowed credentials

checkCache :: OAuthRules -> Set.Set Text -> Set.Set Text -> Credentials -> Expectation
checkCache rules required allowed credentials = do
  calls <- newMVar (0 :: Int)
  clock <- newMVar (0 :: Double)
  let send _ = do
        modifyMVar_ calls (pure . (+ 1))
        threadDelay 1000
        pure (Right (object ["access_token" .= ("token" :: Text), "expires_in" .= (3600 :: Int)]))
  acquire <- tokenSourceWithClock send rules required allowed credentials (readMVar clock)
  results <- mapConcurrently (const acquire) [1..5 :: Int]
  results `shouldBe` replicate 5 (Right "token")
  readMVar calls `shouldReturn` 1
  modifyMVar_ clock (const (pure 3539))
  acquire `shouldReturn` Right "token"
  readMVar calls `shouldReturn` 1
  modifyMVar_ clock (const (pure 3540))
  acquire `shouldReturn` Right "token"
  readMVar calls `shouldReturn` 2

authorizationSpec :: Spec
authorizationSpec = do
  it "uses SHA256 PKCE and rejects duplicate/invalid callback state" $ withAuthRules $ \_ rules _ -> do
    pkceChallenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" `shouldBe` "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    let valid = [("state", Just "expected"), ("code", Just "code")]
        accepted = callbackResult rules "expected" "GET" "/" valid
    callbackStatus accepted `shouldBe` status200
    callbackDecision accepted `shouldBe` Just (Right "code")
    callbackDecision (callbackResult rules "expected" "GET" "/" (("state", Just "expected"):valid)) `shouldBe` Nothing
    callbackStatus (callbackResult rules "wrong" "GET" "/" valid) `shouldBe` status400
    callbackDecision (callbackResult rules "expected" "GET" "/" [("state", Just "expected"),("error", Just "denied")]) `shouldBe` Just (Left (failure "authorization_declined"))
  it "requires a selected integration and bounds external callback options" $ withAuthRules $ \_ rules groups -> do
    let args = ["--client-secrets", "client.json", "--no-gmail"]
    void (parseOptions rules (args ++ ["--port", "65536"])) `shouldBe` Left (failure "authorization_port_invalid")
    case parseOptions rules args of
      Left problem -> expectationFailure (show problem)
      Right options -> selectedScopes groups options `shouldBe` Left (failure "authorization_select_integration")
  it "completes loopback consent and writes private credentials using only mocked Google HTTP" $ withAuthRules $ \_ rules groups ->
    withClient rules (checkLoopback rules groups False)
  it "retains existing credentials after declined loopback consent" $ withAuthRules $ \_ rules groups ->
    withClient rules (checkLoopback rules groups True)
  it "builds an exchange request and validates selected scopes from the response" $ withAuthRules $ \_ rules groups -> do
    let scopes = Map.findWithDefault [] "gmail" groups
    withClient rules $ \client -> do
      let response = object ["scope" .= T.unwords scopes, "token_type" .= ("Bearer" :: Text),
            "access_token" .= ("access" :: Text), "refresh_token" .= ("refresh" :: Text), "expires_in" .= (3600 :: Int)]
          now = UTCTime (fromGregorian 2026 1 1) 0
          wire value = Right (Response 200 [] (BL.toStrict (encode value)))
          result = exchangeCode rules client scopes now (wire response)
          request = codeRequest rules groups client scopes "code" "verifier" "http://127.0.0.1:8765/"
          mismatched = field "scope" (String (T.unwords scopes <> " extra")) response
      fmap requestMethod request `shouldBe` Right "POST"
      fmap (objectField "type") result `shouldBe` Right (String "authorized_user")
      exchangeCode rules client scopes now (wire mismatched) `shouldBe` Left (failure "authorization_scopes_mismatch")

withClient :: AuthorizationRules -> (DesktopClient -> Expectation) -> Expectation
withClient rules check = case clientDocument rules document of
  Left problem -> expectationFailure (show problem)
  Right client -> check client
  where
    document :: Value
    document = object ["installed" .= object ["client_id" .= ("client" :: Text), "client_secret" .= ("secret" :: Text)]]

objectField :: Data.Aeson.Key.Key -> Value -> Value
objectField key (Object fields) = fromMaybe Null (KM.lookup key fields)
objectField _ _ = Null

checkLoopback :: AuthorizationRules -> ScopeGroups -> Bool -> DesktopClient -> Expectation
checkLoopback rules groups declined client = withPrivateDirectory $ \directory -> do
  let path = directory </> "token.json"
      arguments = ["--client-secrets", "client.json", "--token-file", path, "--no-browser", "--port", "0", "--timeout", "2"]
  BS.writeFile path "existing credentials"
  case parseOptions rules arguments of
    Left problem -> expectationFailure (show problem)
    Right options -> runLoopback rules groups declined client path options

runLoopback :: AuthorizationRules -> ScopeGroups -> Bool -> DesktopClient -> FilePath -> AuthorizationOptions -> Expectation
runLoopback rules groups declined client path options = do
  threads <- newMVar []
  calls <- newMVar (0 :: Int)
  callback <- newRawSender
  let scopes = Map.findWithDefault [] "gmail" groups
      document = object ["scope" .= T.unwords scopes, "token_type" .= ("bearer" :: Text),
        "access_token" .= ("access" :: Text), "refresh_token" .= ("refresh" :: Text)]
      exchange _ = modifyMVar calls (\count -> pure (count + 1, Right (Response 200 [] (BL.toStrict (encode document)))))
      announce notice = case callbackURL declined notice of
        Nothing -> pure ()
        Just url -> do
          let (base, query) = T.breakOn "?" url
              parameters = parseQuery (T.encodeUtf8 query)
          task <- async (callback (Request "GET" base [] parameters EmptyBody 65536 2))
          modifyMVar_ threads (pure . (task:))
      effects = AuthorizationEffects exchange announce (const (expectationFailure "browser must not open")) (pure (UTCTime (fromGregorian 2026 1 1) 0))
  result <- authorize rules groups options client effects
  tasks <- readMVar threads
  mapM_ wait tasks
  case declined of
    True -> checkDeclined result calls
    False -> checkSaved result calls
  where
    checkDeclined :: Result () -> MVar Int -> Expectation
    checkDeclined result calls = do
      result `shouldBe` Left (failure "authorization_declined")
      BS.readFile path `shouldReturn` "existing credentials"
      readMVar calls `shouldReturn` 0
    checkSaved :: Result () -> MVar Int -> Expectation
    checkSaved result calls = do
      result `shouldBe` Right ()
      readMVar calls `shouldReturn` 1
      stored <- BS.readFile path
      fmap (objectField "type") (eitherDecodeStrict' stored) `shouldBe` Right (String "authorized_user")
      metadata <- getFileStatus path
      fileMode metadata .&. accessModes `shouldBe` 0o600

callbackURL :: Bool -> Text -> Maybe Text
callbackURL declined notice = do
  url <- case reverse (T.lines notice) of
    candidate:_ | "https://" `T.isPrefixOf` candidate -> Just candidate
    _ -> Nothing
  let query = parseQuery (T.encodeUtf8 (snd (T.breakOn "?" url)))
  Just redirect <- lookup "redirect_uri" query
  Just state <- lookup "state" query
  let result
        | declined = ("error", Just "access_denied")
        | otherwise = ("code", Just "mock-code")
      callbackQuery = renderQuery True [("state", Just state), result]
  pure (T.decodeUtf8 (redirect <> callbackQuery))

withPrivateDirectory :: (FilePath -> Expectation) -> Expectation
withPrivateDirectory = bracket create removeDirectoryRecursive
  where
    create :: IO FilePath
    create = do
      root <- getTemporaryDirectory
      (path, handle) <- openTempFile root "baerbel-oauth-"
      hClose handle
      removeFile path
      createDirectory path
      setFileMode path 0o700
      pure path
