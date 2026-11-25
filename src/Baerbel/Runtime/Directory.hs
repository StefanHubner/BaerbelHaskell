module Baerbel.Runtime.Directory (module Baerbel.Core.Directory, LookupCaller, withDirectory) where

import Baerbel.Core.Directory
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( decodeValue )
import Baerbel.Runtime.Effects ( monotonicSeconds )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.STM
    ( atomically,
      check,
      TVar,
      newTVarIO,
      readTVar,
      readTVarIO,
      modifyTVar' )
import Control.Monad ( forever, void )
import Data.Text ( Text )
import System.Timeout ( timeout )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type LookupCaller = Text -> IO (Maybe Text)
loadDirectory :: DirectoryRules -> Send -> IO (Result Index)
loadDirectory rules sender = loadPage rules sender 0 Nothing Set.empty Map.empty

loadPage :: DirectoryRules -> Send -> Int -> Maybe Text -> Set.Set Text -> Index -> IO (Result Index)
loadPage rules sender number token seen index
  | number >= directoryPages rules = pure (Left (failure "incoming_contacts_directory_limit"))
  | otherwise = do
    let request = directoryRequest rules token
    response <- requestJSON sender request (decodeValue "incoming_contacts_invalid_response")
    case response >>= advanceDirectory rules number seen index of
      Left issue -> pure (Left issue)
      Right (updated, Nothing) -> pure (Right updated)
      Right (updated, Just next) -> loadPage rules sender (number + 1) (Just next) (Set.insert next seen) updated


withDirectory :: DirectoryRules -> Maybe Send -> (LookupCaller -> IO (Result a)) -> IO (Result a)
withDirectory _ Nothing action = action (const (pure Nothing))
withDirectory rules (Just sender) action = do
  state <- newTVarIO (Snapshot Map.empty Nothing False)
  withAsync (forever (refresh rules sender state >> threadDelay (directoryRefreshMicros rules))) (const (action (lookupCaller rules state)))

refresh :: DirectoryRules -> Send -> TVar Snapshot -> IO ()
refresh rules sender state = do
  result <- timeout (directoryRefreshTimeoutMicros rules) (loadDirectory rules sender)
  now <- monotonicSeconds
  atomically (modifyTVar' state (refreshSnapshot now result))

lookupCaller :: DirectoryRules -> TVar Snapshot -> LookupCaller
lookupCaller rules state number = do
  void (timeout (directoryReadyMicros rules) (atomically (readTVar state >>= check . snapshotReady)))
  now <- monotonicSeconds
  current <- readTVarIO state
  pure (snapshotName rules now number current)
