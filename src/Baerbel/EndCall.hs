{-# LANGUAGE OverloadedStrings #-}
module Baerbel.EndCall
  ( EndConfig(..), EndContext(..), EndEffects(..), EndReason(..)
  , parseEndReason, checkEndRequest, executeEndCall
  ) where

import Baerbel.Error (Result, failure)
import Baerbel.JSON (argumentOptions, decodeValue, jsonOptions, resultValue)
import Data.Aeson (Value, FromJSON(..), Options(..), camelTo2, genericParseJSON)
import GHC.Generics (Generic)
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

data EndReason = OwnerRequested | ConversationComplete deriving (Eq, Show, Generic)
instance FromJSON EndReason where
  parseJSON = genericParseJSON ((jsonOptions 0) {constructorTagModifier = camelTo2 '_'})

newtype EndArgs = EndArgs {endReason :: EndReason} deriving (Generic)
instance FromJSON EndArgs where parseJSON = genericParseJSON (argumentOptions 3)

parseEndReason :: Value -> Result EndReason
parseEndReason value = endReason <$> decodeValue "invalid_arguments" value

checkEndRequest :: EndContext -> EndReason -> Result EndReason
checkEndRequest context reason
  | not (endOwner context) || endPaused context = Left (failure "end_call_unavailable")
  | otherwise = checkPending context reason

checkPending :: EndContext -> EndReason -> Result EndReason
checkPending context ConversationComplete
  | endPinPending context || endNotesPending context || endOtherWork context =
      Left (failure "finish_pending_work_before_ending_call")
checkPending _ reason = Right reason

executeEndCall :: EndConfig -> IO EndContext -> EndEffects -> EndReason -> IO (Result Value)
executeEndCall cfg snapshot effects reason = do
  context <- snapshot
  case checkEndRequest context reason of
    Left err -> pure (Left err)
    Right _ -> completeCall cfg effects

completeCall :: EndConfig -> EndEffects -> IO (Result Value)
completeCall cfg effects = do
  let farewell = announceFarewell effects (endFarewellText cfg) (endCompletionText cfg)
  spoken <- timeout (endTimeoutMicros cfg) farewell
  case spoken of
    Just (Right False) -> pure (Right (resultValue "caller_interrupted_continue_conversation"))
    _ -> do
      _ <- terminateCall effects
      markEnded effects
      pure (Right (resultValue "call_ending"))
