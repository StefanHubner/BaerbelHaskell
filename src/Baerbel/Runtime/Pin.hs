{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Pin
  ( PinConfig(..), PinState(..), PinGate(..), emptyPinState, keypadStep
  , parsePinHash, verifyPin, newPinGate
  ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.Pin
import Control.Concurrent.Async ( Async, async, cancel )
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar_ )
import Control.Concurrent.STM
    ( atomically,
      retry,
      STM,
      TVar,
      newTVarIO,
      readTVar,
      readTVarIO,
      writeTVar,
      modifyTVar',
      stateTVar )
import Control.Exception ( evaluate, mask, finally )
import Control.Monad ( when, unless )
import Data.Aeson ( Value )
import Data.Text ( Text )
import GHC.Clock ( getMonotonicTimeNSec )
import System.Timeout ( timeout )
import qualified Data.ByteString as BS
import qualified Baerbel.Runtime.Effects as Effects
import qualified Data.Text as T

type PinTask = MVar (Maybe (Async ()))

data PinGate = PinGate
  { feedDigit :: BS.ByteString -> IO ()
  , collecting :: IO Bool
  , prepareTool :: (Value -> IO (Result ())) -> Text -> IO (Result ())
  , closePin :: IO ()
  }

newPinGate :: PinConfig -> IO (Result PinGate)
newPinGate cfg
  | T.null (pinEncoded cfg) = pure (Right disabledGate)
  | otherwise = case parsePinHash (pinEncoded cfg) of
      Left err -> pure (Left err)
      Right encoded -> createGate cfg encoded

disabledGate :: PinGate
disabledGate = PinGate (const (pure ())) (pure False) (\_ _ -> pure (Right ())) (pure ())

createGate :: PinConfig -> PinHash -> IO (Result PinGate)
createGate cfg encoded = do
  state <- newTVarIO emptyPinState
  deadline <- newTVarIO 0
  closed <- newTVarIO False
  notifier <- newTVarIO Nothing
  checking <- newTVarIO False
  task <- newMVar Nothing
  let active = isCollecting state deadline closed
      feed = handleDigit cfg encoded state deadline closed notifier checking task
      guard send name = atomically (writeTVar notifier (Just send)) >> protectTool cfg state deadline closed send name
      close = do
        atomically (writeTVar closed True >> writeTVar state emptyPinState >> writeTVar deadline 0)
        modifyMVar_ task (\worker -> maybe (pure ()) cancel worker >> pure Nothing)
  pure (Right (PinGate feed active guard close))

nowMicros :: IO Integer
nowMicros = do
  nanos <- getMonotonicTimeNSec
  pure (fromIntegral nanos `div` 1000)

isCollecting :: TVar PinState -> TVar Integer -> TVar Bool -> IO Bool
isCollecting state deadline closed = do
  now <- nowMicros
  current <- readTVarIO state
  expires <- readTVarIO deadline
  stopped <- readTVarIO closed
  pure (not stopped && not (pinUnlocked current) && now < expires)

handleDigit :: PinConfig -> PinHash -> TVar PinState -> TVar Integer -> TVar Bool -> TVar (Maybe (Value -> IO (Result ()))) -> TVar Bool -> PinTask -> BS.ByteString -> IO ()
handleDigit cfg encoded state deadline closed notifier checking task digit = do
  active <- isCollecting state deadline closed
  busy <- readTVarIO checking
  when (active && not busy) submit
  where
    submit = do
      submission <- atomically (applyDigit (pinMaximumDigits cfg) state digit)
      let verify digits = launchCheck closed checking task (checkSubmission cfg encoded state deadline closed notifier digits)
      maybe (pure ()) verify submission

launchCheck :: TVar Bool -> TVar Bool -> PinTask -> IO () -> IO ()
launchCheck closed checking task action = mask $ \restore ->
  modifyMVar_ task (startCheck closed checking (restore action))

startCheck :: TVar Bool -> TVar Bool -> IO () -> Maybe (Async ()) -> IO (Maybe (Async ()))
startCheck closed checking action previous = do
  allowed <- atomically (claimCheck closed checking)
  let start | allowed = Just <$> async (finally action cleanup)
            | otherwise = pure previous
      cleanup = atomically (writeTVar checking False)
  start

claimCheck :: TVar Bool -> TVar Bool -> STM Bool
claimCheck closed checking = do
  stopped <- readTVar closed
  busy <- readTVar checking
  let allowed = not (stopped || busy)
  when allowed (writeTVar checking True)
  pure allowed

applyDigit :: Int -> TVar PinState -> BS.ByteString -> STM (Maybe BS.ByteString)
applyDigit limit state digit = stateTVar state apply
  where
    apply current = let (updated, submission) = keypadStep limit digit current
                    in (submission, updated)

checkSubmission :: PinConfig -> PinHash -> TVar PinState -> TVar Integer -> TVar Bool -> TVar (Maybe (Value -> IO (Result ()))) -> BS.ByteString -> IO ()
checkSubmission cfg encoded state deadline closed notifier digits = do
  now <- nowMicros
  let validLength = BS.length digits >= pinMinimumDigits cfg && BS.length digits <= pinMaximumDigits cfg
      checked = validLength && verifyPin encoded digits
      renewed = now + fromIntegral (pinWindowMicros cfg)
  verified <- evaluate checked
  atomically $ do
    stopped <- readTVar closed
    let (updated, expires) = verifiedState renewed verified stopped
    writeTVar state updated
    writeTVar deadline expires
  unless verified (notifyIncorrect cfg state deadline closed notifier)

notifyIncorrect :: PinConfig -> TVar PinState -> TVar Integer -> TVar Bool -> TVar (Maybe (Value -> IO (Result ()))) -> IO ()
notifyIncorrect cfg state deadline closed notifier = do
  send <- readTVarIO notifier
  stopped <- readTVarIO closed
  let notify | stopped = pure (Right ())
             | otherwise = maybe (pure (Left (failure "pin_unavailable"))) ($ pinIncorrect cfg) send
  result <- notify
  case result of
    Right () -> pure ()
    Left _ -> atomically (writeTVar state emptyPinState >> writeTVar deadline 0)

protectTool :: PinConfig -> TVar PinState -> TVar Integer -> TVar Bool -> (Value -> IO (Result ())) -> Text -> IO (Result ())
protectTool cfg state deadline closed send name = do
  current <- readTVarIO state
  stopped <- readTVarIO closed
  let protect
        | stopped = pure (Left (failure "pin_unavailable"))
        | pinUnlocked current || not (any (`T.isPrefixOf` name) (pinPrefixes cfg)) = pure (Right ())
        | otherwise = requestPin cfg state deadline closed send
  protect

requestPin :: PinConfig -> TVar PinState -> TVar Integer -> TVar Bool -> (Value -> IO (Result ())) -> IO (Result ())
requestPin cfg state deadline closed send = do
  now <- nowMicros
  let expires = now + fromIntegral (pinWindowMicros cfg)
  atomically (writeTVar state emptyPinState >> writeTVar deadline expires)
  prompted <- send (pinPrompt cfg)
  Effects.continue (const (waitVerified state deadline closed)) prompted

waitVerified :: TVar PinState -> TVar Integer -> TVar Bool -> IO (Result ())
waitVerified state deadline closed = do
  now <- nowMicros
  expires <- readTVarIO deadline
  let remaining = max 1 (fromIntegral (expires - now))
  result <- timeout remaining (atomically (awaitPin state closed))
  maybe (pinExpired state deadline closed) pure result

awaitPin :: TVar PinState -> TVar Bool -> STM (Result ())
awaitPin state closed = do
  stopped <- readTVar closed
  current <- readTVar state
  let await
        | stopped = pure (Left (failure "pin_unavailable"))
        | pinUnlocked current = pure (Right ())
        | otherwise = retry
  await

pinExpired :: TVar PinState -> TVar Integer -> TVar Bool -> IO (Result ())
pinExpired state deadline closed = do
  active <- isCollecting state deadline closed
  let finish | active = waitVerified state deadline closed
             | otherwise = expire
  finish
  where
    expire = do
      atomically (modifyTVar' state (\current -> current {pinDigits = BS.empty}))
      pure (Left (failure "pin_entry_expired"))
