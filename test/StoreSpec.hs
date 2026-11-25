module StoreSpec (spec) where

import Baerbel.Runtime.Worker (withWorker)
import Control.Concurrent.Async (Async, withAsync, async, cancel, wait)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import System.Timeout (timeout)
import Baerbel.Runtime.Config (Settings, loadSettings, rule)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Runtime.Profile (ProfileState(..), ProfileStore(..), emptyProfile)
import Baerbel.Runtime.Store (StoreRules(..), profileStore, validateProfileFile)
import Baerbel.Core.JSON (encodeBytes, decodeUnique)
import Data.Aeson (Value, object)
import qualified Data.ByteString as Bytes
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode, createSymbolicLink, createLink)
import Test.Hspec

spec :: Spec
spec = do
  describe "profile filesystem boundaries" $ do
    it "rejects duplicate keys, including escaped spellings" $
      (decodeUnique "profile_invalid_state" "{\"a\":1,\"\\u0061\":2}" :: Result Value) `shouldBe` Left (failure "profile_invalid_state")
    it "allows an absent profile in an existing private directory" $ withStoreConfig $ \settings directory -> do
      let path = directory </> "profile.json"
      validate settings path `shouldReturn` Right ()
    it "reads typed snapshots without creating a mutation lock" $ withStoreConfig readWithoutLock
    it "does not write unchanged state even when the byte limit is smaller" $
      withStoreConfig (withoutWrite emptyProfile (Right (object [])))
    it "rejects oversized updates before writing state" $
      withStoreConfig (withoutWrite oversized (Left (failure "profile_storage_limit_reached")))
    it "rejects a public parent directory" $ withStoreConfig $ \settings directory -> do
      setFileMode directory 0o755
      validate settings (directory </> "profile.json") `shouldReturn` Left (failure "profile_unsafe_storage")
    it "rejects symlink and hardlink profile aliases" $ withStoreConfig $ \settings directory -> do
      let path = directory </> "profile.json"
          alias = directory </> "alias.json"
          hard = directory </> "hard.json"
      Bytes.writeFile path (encodeBytes emptyProfile)
      setFileMode path 0o600
      createSymbolicLink path alias
      validate settings alias `shouldReturn` Left (failure "profile_storage_unavailable")
      createLink path hard
      validate settings hard `shouldReturn` Left (failure "profile_unsafe_storage")
    it "rejects profile timestamps that Python datetime rejects" $ withStoreConfig $ \settings directory -> do
      let path = directory </> "profile.json"
          invalid = ["0000-01-01T00:00:00Z", "2026-12-31T23:59:60Z", " 026-01-01T00:00:00Z"]
          check stamp = do
            Bytes.writeFile path (encodeBytes (ProfileState 1 Map.empty (Just stamp)))
            setFileMode path 0o600
            validate settings path `shouldReturn` Left (failure "profile_invalid_state")
      mapM_ check invalid
    it "rejects decimal and exponent encodings of the persisted integer version" $ withStoreConfig $ \settings directory -> do
      let path = directory </> "profile.json"
          check number = do
            Bytes.writeFile path ("{\"version\":" <> number <> ",\"values\":{},\"updated_at\":null}")
            setFileMode path 0o600
            validate settings path `shouldReturn` Left (failure "profile_invalid_state")
      mapM_ check ["1.0", "1e0", "1E+0"]
    it "drains an accepted profile save even after its caller stops waiting" $ withStoreConfig acceptedSave
    it "preserves a coherent Unicode snapshot after atomic replacement" $ withStoreConfig $ \settings directory -> do
      let path = directory </> "profile.json"
          state = ProfileState 1 (Map.singleton "home" "München") (Just "2026-11-19T12:00:00Z")
      case (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store" of
        Left issue -> expectationFailure (show issue)
        Right (profileRules, storeRules) -> do
          prepared <- profileStore profileRules storeRules path
          either (expectationFailure . show) (verify path state) prepared
  where
  oversized = ProfileState 1 (Map.singleton "home" "Vienna") (Just "2026-11-19T12:00:00Z")
  verify path state store = do
    result <- profileUpdate store (\_ -> Right (state, object []))
    result `shouldBe` Right (object [])
    profileRead store `shouldReturn` Right state
    raw <- Bytes.readFile path
    (decodeUnique "invalid" raw :: Result ProfileState) `shouldBe` Right state

withoutWrite :: ProfileState -> Result Value -> Settings -> FilePath -> IO ()
withoutWrite state expected settings directory = either (expectationFailure . show) open configured
  where
    path = directory </> "profile.json"
    configured = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
    open (profileRules, storeRules) = do
      let bounded = storeRules {storeMaximumBytes = 1}
      prepared <- profileStore profileRules bounded path
      either (expectationFailure . show) verify prepared
    verify store = do
      profileUpdate store (const (Right (state, object []))) `shouldReturn` expected
      doesFileExist path `shouldReturn` False

readWithoutLock :: Settings -> FilePath -> IO ()
readWithoutLock settings directory = either (expectationFailure . show) open configured
  where
    path = directory </> "profile.json"
    configured = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
    open (profileRules, storeRules) = do
      prepared <- profileStore profileRules storeRules path
      either (expectationFailure . show) verify prepared
    verify store = do
      profileRead store `shouldReturn` Right emptyProfile
      doesFileExist (path <> ".lock") `shouldReturn` False

withStoreConfig :: (Settings -> FilePath -> IO ()) -> IO ()
withStoreConfig action = do
  loaded <- loadSettings "config/global.yaml" "config/user.example.json"
  case loaded of
    Left issue -> expectationFailure (show issue)
    Right settings -> withSystemTempDirectory "baerbel-profile" $ \directory -> setFileMode directory 0o700 >> action settings directory

validate :: Settings -> FilePath -> IO (Result ())
validate settings path = case (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store" of
  Left issue -> pure (Left issue)
  Right (profileRules, storeRules) -> validateProfileFile profileRules storeRules path

acceptedSave :: Settings -> FilePath -> IO ()
acceptedSave settings directory = do
  let configured = (,) <$> rule settings "providers" "profile" <*> rule settings "profile" "store"
  either (expectationFailure . show) open configured
  where
    path = directory </> "accepted.json"
    state = ProfileState 1 (Map.singleton "home" "Vienna") (Just "2026-11-19T12:00:00Z")
    open (profileRules, storeRules) = do
      prepared <- profileStore profileRules storeRules path
      either (expectationFailure . show) exercise prepared
    exercise store = do
      started <- newEmptyMVar
      release <- newEmptyMVar
      waiterGone <- newEmptyMVar
      let operation () = putMVar started () >> takeMVar release >> profileUpdate store (\_ -> Right (state, object []))
          scope = withWorker operation (cancelWaiter started waiterGone)
      withAsync scope (verifyDrain release waiterGone)
    verifyDrain :: MVar () -> MVar () -> Async (Result ()) -> IO ()
    verifyDrain release waiterGone task = do
      takeMVar waiterGone
      timeout 20000 (wait task) `shouldReturn` Nothing
      putMVar release ()
      wait task `shouldReturn` Right ()
      raw <- Bytes.readFile path
      (decodeUnique "invalid" raw :: Result ProfileState) `shouldBe` Right state

cancelWaiter :: MVar () -> MVar () -> (() -> IO (Result Value)) -> IO (Result ())
cancelWaiter started gone submit = do
  task <- async (submit ())
  takeMVar started
  cancel task
  putMVar gone ()
  pure (Right ())
