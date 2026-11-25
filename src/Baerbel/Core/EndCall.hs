{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.EndCall where

import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (argumentOptions, decodeValue, jsonOptions)
import Data.Aeson (Value, FromJSON(..), Options(..), camelTo2, genericParseJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

data EndConfig = EndConfig
  { endFarewellText :: Text, endCompletionText :: Text, endTimeoutMicros :: Int
  } deriving (Eq, Show)

data EndContext = EndContext
  { endOwner :: Bool, endPaused :: Bool, endPinPending :: Bool
  , endNotesPending :: Bool, endOtherWork :: Bool
  } deriving (Eq, Show)

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
