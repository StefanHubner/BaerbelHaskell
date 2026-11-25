{-# LANGUAGE ForeignFunctionInterface #-}
module Baerbel.Runtime.Store (StoreRules(..), profileStore, validateProfileFile) where

import Baerbel.Core.ProviderConfig ( ProfileRules(..) )
import Baerbel.Core.Store
    ( StoreRules(..),
      validatePath,
      parents,
      boundedState,
      prepareSave,
      storageFailure )
import Baerbel.Runtime.Error ( Result, failure )
import Baerbel.Runtime.Profile
    ( ProfileState, emptyProfile, ProfileStore(ProfileStore) )
import Baerbel.Runtime.Storage ( atomicWrite )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.MVar ( newMVar, withMVar )
import Control.Exception ( IOException, bracket, try )
import Data.Bifunctor ( first )
import Data.Bits ( (.&.) )
import Foreign.C.Types ( CInt(..) )
import System.FilePath ( takeDirectory )
import System.IO ( hClose )
import System.IO.Error ( isDoesNotExistError )
import System.Posix.Files
    ( getFileStatus,
      getSymbolicLinkStatus,
      fileMode,
      fileOwner,
      getFdStatus,
      isDirectory,
      isRegularFile,
      isSymbolicLink,
      linkCount,
      FileStatus )
import System.Posix.IO
    ( OpenMode(..),
      OpenFileFlags(..),
      defaultFileFlags,
      openFd,
      closeFd,
      fdToHandle,
      dup )
import System.Posix.Types ( Fd(..), UserID )
import System.Posix.User ( getEffectiveUserID )
import System.Timeout ( timeout )
import qualified System.Posix.Types ( FileMode )
import qualified Data.ByteString as Bytes ( hGet )
import qualified Baerbel.Runtime.Effects as Effects
    ( continue, attempt )

foreign import ccall unsafe "flock" fileLock :: CInt -> CInt -> IO CInt

profileStore :: ProfileRules -> StoreRules -> FilePath -> IO (Result ProfileStore)
profileStore profileRules rules path = do
  checked <- validateProfileFile profileRules rules path
  gate <- newMVar ()
  let operation step = withMVar gate (const (locked profileRules rules path step))
      store = ProfileStore (readCurrentProfile profileRules rules path) operation
  pure (store <$ checked)

readCurrentProfile :: ProfileRules -> StoreRules -> FilePath -> IO (Result ProfileState)
readCurrentProfile profileRules rules path = do
  owner <- inspectOwner path
  Effects.continue (readProfile profileRules rules path) owner

validateProfileFile :: ProfileRules -> StoreRules -> FilePath -> IO (Result ())
validateProfileFile profileRules rules path = Effects.continue inspect (validatePath path)
  where
  inspect () = fmap (const () <$>) (readCurrentProfile profileRules rules path)

inspectOwner :: FilePath -> IO (Result UserID)
inspectOwner path = Effects.attempt "profile_storage_unavailable" inspect
  where
    parent = takeDirectory path
    inspect = do
      ancestors <- traverse getSymbolicLinkStatus (parents parent)
      metadata <- getFileStatus parent
      profileOwner ancestors metadata <$> getEffectiveUserID

profileOwner :: [FileStatus] -> FileStatus -> UserID -> Result UserID
profileOwner ancestors metadata effective
  | safe = Right (fileOwner metadata)
  | otherwise = Left (failure "profile_unsafe_storage")
  where
    safe = not (any isSymbolicLink ancestors) && isDirectory metadata && permissions metadata == 0o700
      && (effective == 0 || effective == fileOwner metadata)

permissions :: FileStatus -> System.Posix.Types.FileMode
permissions metadata = fileMode metadata .&. 0o7777

privateRegular :: UserID -> Bool -> FileStatus -> Bool
privateRegular owner unlinked metadata = isRegularFile metadata && fileOwner metadata == owner
  && permissions metadata == 0o600 && (linkCount metadata == 1 || unlinked && linkCount metadata == 0)

locked :: ProfileRules -> StoreRules -> FilePath -> (ProfileState -> Result (ProfileState, a)) -> IO (Result a)
locked profileRules rules path operation = do
  owner <- inspectOwner path
  Effects.continue protect owner
  where
  flags = defaultFileFlags {creat = Just 0o600, nofollow = True, nonBlock = True, cloexec = True}
  protect uid = Effects.attempt "profile_storage_unavailable" (bracket (openFd (path ++ ".lock") ReadWrite flags) closeFd (run uid))
  run uid descriptor = withPrivateDescriptor uid False descriptor (acquire uid descriptor)
  acquire uid descriptor = do
    obtained <- timeout (storeLockMicros rules) (awaitLock rules descriptor)
    maybe (pure (Left (failure "profile_busy_try_again"))) (const (update profileRules rules path uid operation)) obtained

withPrivateDescriptor :: UserID -> Bool -> Fd -> IO (Result a) -> IO (Result a)
withPrivateDescriptor owner unlinked descriptor action = getFdStatus descriptor >>= use
  where
    use metadata
      | privateRegular owner unlinked metadata = action
      | otherwise = pure (Left (failure "profile_unsafe_storage"))

awaitLock :: StoreRules -> Fd -> IO ()
awaitLock rules descriptor@(Fd raw) = fileLock raw 6 >>= continue
  where
    continue 0 = pure ()
    continue _ = threadDelay (storePollMicros rules) >> awaitLock rules descriptor

update :: ProfileRules -> StoreRules -> FilePath -> UserID -> (ProfileState -> Result (ProfileState, a)) -> IO (Result a)
update profileRules rules path uid operation = do
  previous <- readProfile profileRules rules path uid
  case previous >>= prepareSave rules operation of
    Left issue -> pure (Left issue)
    Right (bytes, result) -> do
      written <- traverse (atomicWrite path) bytes
      pure (result <$ first storageFailure (sequence_ written))

readProfile :: ProfileRules -> StoreRules -> FilePath -> UserID -> IO (Result ProfileState)
readProfile profileRules rules path owner = Effects.attempt "profile_storage_unavailable" openedFile
  where
  openedFile = do
    opened <- try (openFd path ReadOnly flags)
    either (pure . missingProfile) (\descriptor -> bracket (pure descriptor) closeFd readOwned) opened
  flags = defaultFileFlags {nofollow = True, nonBlock = True, cloexec = True}
  readOwned descriptor = withPrivateDescriptor owner True descriptor (readBytes descriptor)
  readBytes descriptor = do
    copy <- dup descriptor
    let bound = storeMaximumBytes rules + 1
    raw <- bracket (fdToHandle copy) hClose (`Bytes.hGet` bound)
    pure (boundedState profileRules rules raw)

missingProfile :: IOException -> Result ProfileState
missingProfile issue
  | isDoesNotExistError issue = Right emptyProfile
  | otherwise = Left (failure "profile_storage_unavailable")
