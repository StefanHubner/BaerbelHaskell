module Baerbel.Runtime.UserDatabase
  (readAccounts, updatePin, googleCredentials, saveGoogleCredentials, accountMemory) where

import Baerbel.Core.Accounts (Account(..), Plan(..))
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.OAuth (Credentials(..))
import Baerbel.Core.Profile (ProfileState(..))
import Baerbel.Core.Store (StoreRules, saveContent)
import Baerbel.Runtime.Effects (continue)
import Baerbel.Runtime.Profile (ProfileStore(..))
import Baerbel.Runtime.Storage (AccountStore(..), withAccountDatabase, withUserDatabase)
import Data.Maybe (listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Database.SQLite.Simple
    (Connection, Only(..), changes, execute, executeMany, query, query_, withImmediateTransaction, withTransaction)

readAccounts :: FilePath -> IO (Result (Map Text Account))
readAccounts path = withUserDatabase "user_database_unavailable" path readRows
  where
    readRows connection = withTransaction connection $ do
      users <- query_ connection "SELECT id, user_type, name, phone, pin_hash FROM users"
      settings <- query_ connection "SELECT user_id, name, value FROM user_settings"
      let values = Map.fromListWith Map.union [(ident, Map.singleton key value) | (ident, key, value) <- settings]
      pure (Map.fromList <$> traverse (account values) users)

account :: Map Text (Map Text Text) -> (Text, Text, Text, Text, Text) -> Result (Text, Account)
account settings (ident, kind, name, phone, pin) = build <$> plan
  where
    values = Map.insert "PHONE_ASSISTANT_PRIVATE_PIN_HASH" pin (Map.findWithDefault Map.empty ident settings)
    build typeOfUser = (ident, Account typeOfUser name phone values)
    plan = case kind of
      "Standard" -> Right Standard
      "Pro" -> Right Pro
      _ -> Left (failure "invalid_user_type")

updatePin :: AccountStore -> Text -> IO (Result ())
updatePin store hash = withAccountDatabase "pin_storage_unavailable" store save
  where
    save connection = do
      execute connection "UPDATE users SET pin_hash=? WHERE id=?" (hash, accountStoreId store)
      changed <- changes connection
      pure (oneUser changed)
    oneUser 1 = Right ()
    oneUser _ = Left (failure "unknown_account")

googleCredentials :: AccountStore -> Text -> IO (Result (Maybe Credentials))
googleCredentials store service = withAccountDatabase "google_credentials_unavailable" store readRows
  where
    readRows connection = withTransaction connection $ do
      rows <- query connection sql (user, service)
      Right <$> traverse (credential connection) (listToMaybe rows)
    sql = "SELECT grant_id, client_id, client_secret, refresh_token FROM google_connections JOIN google_grants USING (user_id, grant_id) JOIN oauth_clients USING (client_id) WHERE user_id=? AND service=?"
    credential :: Connection -> (Int, Text, Text, Text) -> IO Credentials
    credential connection (grant, client, secret, token) = do
      scopes <- query connection "SELECT scope FROM google_grant_scopes WHERE user_id=? AND grant_id=?" (user, grant)
      let allowed = Set.fromList (map fromOnly scopes)
      pure (Credentials client secret token allowed Nothing "authorized_user")
    user = accountStoreId store

saveGoogleCredentials :: AccountStore -> Text -> Credentials -> IO (Result ())
saveGoogleCredentials store service credentials = withAccountDatabase "google_credentials_unavailable" store save
  where
    save connection = withImmediateTransaction connection $ do
      execute connection "INSERT INTO oauth_clients VALUES (?, ?) ON CONFLICT(client_id) DO UPDATE SET client_secret=excluded.client_secret"
        (clientId credentials, clientSecret credentials)
      execute connection "INSERT OR IGNORE INTO google_grants VALUES (?, (SELECT COALESCE(MAX(grant_id), 0) + 1 FROM google_grants WHERE user_id=?), ?, ?)"
        (user, user, clientId credentials, refreshToken credentials)
      grants <- query connection "SELECT grant_id FROM google_grants WHERE user_id=? AND client_id=? AND refresh_token=?" identity
      saveConnection connection grants
    saveConnection :: Connection -> [Only Int] -> IO (Result ())
    saveConnection connection [Only grant] = do
      execute connection "DELETE FROM google_grant_scopes WHERE user_id=? AND grant_id=?" (user, grant)
      let scopes = [(user, grant, scope) | scope <- Set.toList (grantedScopes credentials)]
      executeMany connection "INSERT INTO google_grant_scopes VALUES (?, ?, ?)" scopes
      execute connection "INSERT INTO google_connections VALUES (?, ?, ?) ON CONFLICT(user_id, service) DO UPDATE SET grant_id=excluded.grant_id"
        (user, service, grant)
      execute connection "DELETE FROM google_grants WHERE user_id=? AND NOT EXISTS (SELECT 1 FROM google_connections WHERE user_id=google_grants.user_id AND grant_id=google_grants.grant_id)"
        (Only user)
      pure (Right ())
    saveConnection _ _ = pure (Left (failure "google_grant_unavailable"))
    identity = (user, clientId credentials, refreshToken credentials)
    user = accountStoreId store

accountMemory :: StoreRules -> AccountStore -> ProfileStore
accountMemory rules store = ProfileStore readMemory updateMemory
  where
    withMemory = withAccountDatabase "profile_storage_unavailable" store
    readMemory = withMemory (\connection -> withTransaction connection (readProfile connection ident))
    updateMemory operation = withMemory (write operation)
    write operation connection = withImmediateTransaction connection $ do
      previous <- readProfile connection ident
      continue (save connection operation) previous
    save connection operation previous = continue persist prepared
      where
        prepared = do
          (next, result) <- operation previous
          changed <- saveContent rules previous next
          pure (next, result, changed)
        persist (_, result, Nothing) = pure (Right result)
        persist (next, result, Just _) = saveProfile connection ident next result
    ident = accountStoreId store

readProfile :: Connection -> Text -> IO (Result ProfileState)
readProfile connection ident = do
  metadata <- query connection "SELECT memory_updated_at FROM users WHERE id=?" (Only ident)
  entries <- query connection "SELECT key, value FROM user_memory WHERE user_id=?" (Only ident)
  pure (build metadata entries)
  where
    build [Only updated] entries = Right (ProfileState 1 (Map.fromList entries) updated)
    build _ _ = Left (failure "unknown_account")

saveProfile :: Connection -> Text -> ProfileState -> a -> IO (Result a)
saveProfile connection ident state result = do
  execute connection "UPDATE users SET memory_updated_at=? WHERE id=?" (profileUpdatedAt state, ident)
  execute connection "DELETE FROM user_memory WHERE user_id=?" (Only ident)
  executeMany connection "INSERT INTO user_memory VALUES (?, ?, ?)" entries
  pure (Right result)
  where entries = [(ident, key, value) | (key, value) <- Map.toList (profileValues state)]
