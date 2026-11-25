{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.OAuth
  ( module Core, loadCredentials, tokenSource, tokenSourceWithClock ) where

import Baerbel.Core.Error ( Result )
import Baerbel.Core.JSON ( decodeBytes )
import Baerbel.Runtime.Effects
    ( attempt, continue, mapResult, monotonicSeconds )
import Baerbel.Runtime.HTTP ( Send )
import Control.Concurrent.MVar ( modifyMVar, newMVar )
import Control.Monad ( (>=>) )
import Data.Set ( Set )
import Data.Text ( Text )
import System.IO
    ( IOMode(ReadMode), withBinaryFile, hFileSize, Handle )
import Baerbel.Core.OAuth as Core
    ( Cached,
      Scopes(..),
      OAuthRules(..),
      Credentials(..),
      credentialFields,
      credentialSize,
      validateCredentials,
      cachedToken,
      refresh,
      storeToken,
      refreshRequest,
      validateTokenValue,
      checkScopes,
      checkAccess,
      tokenLifetime,
      fromMaybePlus,
      boundedInteger )
import qualified Data.ByteString as BS ( ByteString, hGet, length )

loadCredentials :: OAuthRules -> Set Text -> Set Text -> FilePath -> IO (Result Credentials)
loadCredentials rules required allowed path = mapResult decode readCredentials
  where
    decode = decodeBytes "invalid_credentials" >=> validateCredentials rules required allowed
    readCredentials = attempt "credentials_unavailable" (withBinaryFile path ReadMode readBounded)
    readBounded :: Handle -> IO (Result BS.ByteString)
    readBounded handle = do
      size <- hFileSize handle
      continue (const (readBytes handle)) (credentialSize rules size)
    readBytes :: Handle -> IO (Result BS.ByteString)
    readBytes handle = do
      let limit = oauthCredentialBytes rules + 1
      bytes <- BS.hGet handle limit
      let size = fromIntegral (BS.length bytes)
      pure (bytes <$ credentialSize rules size)

tokenSource :: Send -> OAuthRules -> Set Text -> Set Text -> Credentials -> IO (IO (Result Text))
tokenSource sender rules required allowed credentials = tokenSourceWithClock sender rules required allowed credentials monotonicSeconds

tokenSourceWithClock :: Send -> OAuthRules -> Set Text -> Set Text -> Credentials -> IO Double -> IO (IO (Result Text))
tokenSourceWithClock sender rules required allowed credentials clock = do
  cache <- newMVar Nothing
  pure (modifyMVar cache refreshCached)
  where
    refreshCached :: Cached -> IO (Cached, Result Text)
    refreshCached cached = do
      now <- clock
      let refreshed = refresh rules required allowed cached <$> sender request <*> clock
      maybe refreshed (\value -> pure (cached, Right value)) (cachedToken now cached)
    request = refreshRequest rules credentials
