{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.EndCall
  ( EndConfig(..), EndContext(..), EndEffects(..), EndReason(..)
  , parseEndReason, checkEndRequest, executeEndCall
  ) where

import Baerbel.Core.EndCall
import Baerbel.Core.Error ( Result )
import Baerbel.Core.JSON ( resultValue )
import Data.Aeson ( Value )
import Data.Text ( Text )
import System.Timeout ( timeout )
import qualified Baerbel.Runtime.Effects as Effects

data EndEffects = EndEffects
  { announceFarewell :: Text -> Text -> IO (Result Bool)
  , terminateCall :: IO (Result ())
  , markEnded :: IO ()
  }

executeEndCall :: EndConfig -> IO EndContext -> EndEffects -> EndReason -> IO (Result Value)
executeEndCall cfg snapshot effects reason = do
  context <- snapshot
  Effects.continue (const (completeCall cfg effects)) (checkEndRequest context reason)

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
