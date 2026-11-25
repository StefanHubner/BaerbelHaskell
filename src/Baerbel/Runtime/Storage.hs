module Baerbel.Runtime.Storage (databaseResult, withDatabase, privateFile, atomicWrite) where

import Baerbel.Runtime.Error
    ( Failure(..), Result, failure, ioResult )
import Control.Exception ( bracket, finally, try )
import Control.Monad ( unless )
import Data.Text ( Text )
import Database.SQLite.Simple
    ( Connection, SQLError, open, close, execute_ )
import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      renameFile,
      removeFile )
import System.FilePath ( takeDirectory )
import System.IO ( hClose, hFlush, openBinaryTempFile )
import System.Posix.Files
    ( fileOwner,
      linkCount,
      isRegularFile,
      getFdStatus,
      setFdMode,
      setFileMode )
import System.Posix.IO
    ( OpenMode(..),
      OpenFileFlags(..),
      defaultFileFlags,
      openFd,
      closeFd,
      handleToFd )
import System.Posix.Unistd ( fileSynchronise )
import System.Posix.User ( getEffectiveUserID )
import qualified Data.ByteString as Bytes ( ByteString, hPut )
import qualified Baerbel.Runtime.Effects as Effects
    ( continue, mapResult, attempt )

-- Filesystem and SQL exceptions end at this boundary; cancellation passes through.
databaseResult :: Text -> IO a -> IO (Result a)
databaseResult code operation = Effects.mapResult convert (ioResult code (try operation))
  where
  convert :: Either SQLError a -> Result a
  convert = either (const (Left (failure code))) Right

withDatabase :: Text -> FilePath -> (Connection -> IO (Result a)) -> IO (Result a)
withDatabase code path action = do
  checked <- privateFile path
  Effects.continue (const opened) checked
  where
  opened = Effects.mapResult id (databaseResult code (bracket (open path) close run))
  run connection = execute_ connection "PRAGMA busy_timeout=5000" >> action connection

privateFile :: FilePath -> IO (Result ())
privateFile path = Effects.attempt "private_file_unavailable" prepare
  where
  flags = defaultFileFlags {creat = Just 0o600, nofollow = True, cloexec = True}
  prepare = privateDirectory (takeDirectory path) >> bracket (openFd path ReadWrite flags) closeFd inspect
  inspect descriptor = do
    metadata <- getFdStatus descriptor
    owner <- getEffectiveUserID
    let valid = isRegularFile metadata && fileOwner metadata == owner && linkCount metadata == 1
    secure descriptor valid
  secure descriptor True = setFdMode descriptor 0o600 >> pure (Right ())
  secure _ False = pure (Left (failure "private_file_invalid"))

privateDirectory :: FilePath -> IO ()
privateDirectory path = do
  exists <- doesDirectoryExist path
  unless exists (createDirectoryIfMissing True path >> setFileMode path 0o700)

atomicWrite :: FilePath -> Bytes.ByteString -> IO (Result ())
atomicWrite path content = do
  created <- ioResult "storage_unavailable" (openBinaryTempFile parent ".baerbel-")
  Effects.continue write created
  where
  parent = takeDirectory path
  write (temporary, handle) = finally (prepare temporary handle) (cleanup temporary handle)
  cleanup temporary handle = do
    _ <- ioResult "cleanup_failed" (hClose handle)
    _ <- ioResult "cleanup_failed" (removeFile temporary)
    pure ()
  prepare temporary handle = do
    ready <- ioResult "storage_unavailable" (flush temporary handle)
    Effects.continue (const (commit temporary)) ready
  flush temporary handle = do
    setFileMode temporary 0o600
    Bytes.hPut handle content
    hFlush handle
    descriptor <- handleToFd handle
    bracket (pure descriptor) closeFd fileSynchronise
  commit temporary = do
    result <- ioResult "storage_commit_failed" (renameFile temporary path >> syncDirectory parent)
    pure (either (\issue -> Left issue {failureUncertain = True}) Right result)

syncDirectory :: FilePath -> IO ()
syncDirectory path = bracket (openFd path ReadOnly flags) closeFd fileSynchronise
  where flags = defaultFileFlags {directory = True, nofollow = True, cloexec = True}
