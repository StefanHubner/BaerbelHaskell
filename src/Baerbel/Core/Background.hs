module Baerbel.Core.Background (BackgroundRules(..), Progress(..), progressStep) where

import Baerbel.Core.Concurrent (hasElapsed)
import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.Protocol (appendCommand)
import Data.Aeson (Value, FromJSON(..), genericParseJSON)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

data BackgroundRules = BackgroundRules
  { backgroundPollMicros :: Int, backgroundQuietMicros :: Int, backgroundIntervalMicros :: Int
  , backgroundActiveText :: Text, backgroundIdleText :: Text, backgroundCommentaryText :: Text
  , backgroundRecoveryInstructions :: Text, backgroundRecoveryCommentary :: Text
  } deriving (Eq, Show, Generic)
instance FromJSON BackgroundRules where parseJSON = genericParseJSON (jsonOptions 10)

data Progress = Progress { progressPending :: Bool, progressNext :: Word64 } deriving (Eq, Show)

progressStep :: BackgroundRules -> Word64 -> Word64 -> Bool -> Bool -> Progress -> (Progress, [Value])
progressStep rules now lastSpeech active suppressed state
  | active /= progressPending state = (Progress active quietUntil, [thinking])
  | active && now >= progressNext state && hasElapsed (backgroundQuietMicros rules) now lastSpeech && not suppressed =
      (state {progressNext = intervalUntil}, [appendCommand "session.commentary.append" (backgroundCommentaryText rules)])
  | otherwise = (state, [])
  where
    quiet = fromIntegral (backgroundQuietMicros rules) * 1000
    quietUntil = now + quiet
    intervalUntil = now + fromIntegral (backgroundIntervalMicros rules) * 1000
    content | active = backgroundActiveText rules
            | otherwise = backgroundIdleText rules
    thinking = appendCommand "session.thinking.append" content
