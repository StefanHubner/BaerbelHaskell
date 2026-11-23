{-# LANGUAGE OverloadedStrings #-}
module Baerbel.EndCall
  ( EndConfig(..), EndContext(..), EndEffects(..), EndReason(..)
  , checkEndRequest, executeEndCall
  ) where

import Baerbel.Error (Result, failure)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import System.Timeout (timeout)

data EndConfig = EndConfig
  { endFarewellText :: Text, endCompletionText :: Text, endTimeoutMicros :: Int
  } deriving (Eq, Show)

data EndContext = EndContext
  { endOwner :: Bool, endPaused :: Bool, endPinPending :: Bool
  , endNotesPending :: Bool, endOtherWork :: Bool
  } deriving (Eq, Show)

data EndEffects = EndEffects
  { announceFarewell :: Text -> Text -> IO (Result Bool)
  , terminateCall :: IO (Result ())
  , markEnded :: IO ()
  }

data EndReason = OwnerRequested | ConversationComplete deriving (Eq, Show)

checkEndRequest :: EndContext -> Text -> Value -> Result EndReason
checkEndRequest context name args
  | name /= "end_call" = Left (failure "invalid_arguments")
  | not (endOwner context) || endPaused context = Left (failure "end_call_unavailable")
  | otherwise = parseReason args >>= checkPending context

parseReason :: Value -> Result EndReason
parseReason (Object fields) | KM.size fields == 1 = case KM.lookup "reason" fields of
  Just (String "owner_requested") -> Right OwnerRequested
  Just (String "conversation_complete") -> Right ConversationComplete
  _ -> Left (failure "invalid_arguments")
parseReason _ = Left (failure "invalid_arguments")

checkPending :: EndContext -> EndReason -> Result EndReason
checkPending context ConversationComplete
  | endPinPending context || endNotesPending context || endOtherWork context =
      Left (failure "finish_pending_work_before_ending_call")
checkPending _ reason = Right reason

executeEndCall :: EndConfig -> IO EndContext -> EndEffects -> Text -> Value -> Text -> IO (Result Value)
executeEndCall cfg snapshot effects name args _ = do
  context <- snapshot
  case checkEndRequest context name args of
    Left err -> pure (Left err)
    Right _ -> completeCall cfg effects

completeCall :: EndConfig -> EndEffects -> IO (Result Value)
completeCall cfg effects = do
  let farewell = announceFarewell effects (endFarewellText cfg) (endCompletionText cfg)
  spoken <- timeout (endTimeoutMicros cfg) farewell
  case spoken of
    Just (Right False) -> pure (Right (result "caller_interrupted_continue_conversation"))
    _ -> do
      _ <- terminateCall effects
      markEnded effects
      pure (Right (result "call_ending"))

result :: Text -> Value
result outcome = object ["status" .= ("ok" :: Text), "result" .= outcome]
