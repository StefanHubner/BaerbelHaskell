{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Logging
  ( Report, withLogging, eventRecord, reportOwnerEvent, reportIncomingEvent, projectEvent
  ) where

import Baerbel.Core.JSON ( encodeBytes )
import Baerbel.Core.Logging ( eventRecord, projectEvent )
import Baerbel.Runtime.Error
    ( Failure(..), Result, failure, ioResult )
import Control.Concurrent.MVar
    ( MVar, newMVar, modifyMVar_, readMVar )
import Control.Exception
    ( IOException, bracket, bracketOnError, try )
import Control.Monad ( void )
import Data.Aeson ( Value, KeyValue((.=)) )
import Data.Aeson.Types ( Pair )
import Data.Text ( Text )
import Data.Time ( getCurrentTime )
import System.IO ( Handle, hClose, hFlush, stdout, stderr )
import System.IO.Error ( isDoesNotExistError )
import System.Posix.Files
    ( getFdStatus, getFileStatus, deviceID, fileID, FileStatus )
import System.Posix.IO
    ( OpenMode(WriteOnly),
      OpenFileFlags(..),
      defaultFileFlags,
      openFd,
      closeFd,
      fdToHandle )
import System.Posix.Types ( DeviceID, FileID, Fd )
import qualified Data.ByteString as BS ( ByteString, hPut )
import qualified Data.Text.IO as Text ( hPutStrLn )

type Report = Text -> [Pair] -> IO ()
type Identity = (DeviceID, FileID)
data Watched = Watched {watchedHandle :: Handle, watchedIdentity :: Identity}

withLogging :: Maybe FilePath -> (Report -> IO (Result a)) -> IO (Result a)
withLogging path action = do
  opened <- traverse openLog path
  case sequence opened of
    Left problem -> pure (Left problem)
    Right watched -> bracket (newMVar watched) closeLog (action . report path)

openLog :: FilePath -> IO (Result Watched)
openLog path = ioResult "log_file_unavailable" (openWatched path)

openWatched :: FilePath -> IO Watched
openWatched path = bracketOnError (openFd path WriteOnly flags) closeFd convert
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags {append = True, creat = Just 0o666, cloexec = True}
    convert :: Fd -> IO Watched
    convert descriptor = do
      metadata <- getFdStatus descriptor
      handle <- fdToHandle descriptor
      pure (Watched handle (identity metadata))

identity :: FileStatus -> Identity
identity metadata = (deviceID metadata, fileID metadata)

closeLog :: MVar (Maybe Watched) -> IO ()
closeLog state = readMVar state >>= maybe (pure ()) closeWatched

closeWatched :: Watched -> IO ()
closeWatched watched = void (ioResult "log_close_failed" (hClose (watchedHandle watched)))

report :: Maybe FilePath -> MVar (Maybe Watched) -> Report
report path state name fields = modifyMVar_ state emit
  where
    emit :: Maybe Watched -> IO (Maybe Watched)
    emit watched = do
      now <- getCurrentTime
      let bytes = encodeBytes (eventRecord now name fields) <> "\n"
      writeLog stdout bytes
      writeWatched path watched bytes

writeLog :: Handle -> BS.ByteString -> IO ()
writeLog handle bytes = do
  result <- ioResult "log_write_failed" (BS.hPut handle bytes >> hFlush handle)
  either reportFailure (const (pure ())) result

reportFailure :: Failure -> IO ()
reportFailure problem = void (ioResult "log_diagnostic_failed" (Text.hPutStrLn stderr (failureCode problem)))

writeWatched :: Maybe FilePath -> Maybe Watched -> BS.ByteString -> IO (Maybe Watched)
writeWatched Nothing watched _ = pure watched
writeWatched (Just path) watched bytes = do
  refreshed <- refreshFile path watched
  case refreshed of
    Left problem -> reportFailure problem >> pure watched
    Right current -> writeLog (watchedHandle current) bytes >> pure (Just current)

refreshFile :: FilePath -> Maybe Watched -> IO (Result Watched)
refreshFile path Nothing = openLog path
refreshFile path (Just watched) = do
  current <- try (getFileStatus path)
  case observedIdentity current of
    Left problem -> pure (Left problem)
    Right (Just same) | same == watchedIdentity watched -> pure (Right watched)
    Right _ -> replace
  where
    replace :: IO (Result Watched)
    replace = do
      opened <- openLog path
      either (const (pure ())) (const (closeWatched watched)) opened
      pure opened

observedIdentity :: Either IOException FileStatus -> Result (Maybe Identity)
observedIdentity (Right metadata) = Right (Just (identity metadata))
observedIdentity (Left problem)
  | isDoesNotExistError problem = Right Nothing
  | otherwise = Left (failure "log_file_unavailable")

reportOwnerEvent :: Report -> Text -> Value -> IO ()
reportOwnerEvent = reportProjected True

reportIncomingEvent :: Report -> Text -> Value -> IO ()
reportIncomingEvent = reportProjected False

reportProjected :: Bool -> Report -> Text -> Value -> IO ()
reportProjected owner output identifier event = mapM_ emit (projectEvent owner event)
  where
    emit :: (Text, [Pair]) -> IO ()
    emit (name, fields) = output name (("call_id" .= identifier) : fields)
