module LiveCheck.Config (Development(..), SpeechSettings(..)) where

import Baerbel.Core.JSON (jsonOptions)
import Baerbel.Core.ProviderConfig (HttpPolicy)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Development = Development {developmentSpeech :: SpeechSettings} deriving (Generic)
instance FromJSON Development where parseJSON = genericParseJSON (jsonOptions 11)

data SpeechSettings = SpeechSettings
  { speechModel :: Text, speechVoice :: Text, speechInstructions :: Text
  , speechUrl :: Text, speechHttp :: HttpPolicy } deriving (Generic)
instance FromJSON SpeechSettings where parseJSON = genericParseJSON (jsonOptions 6)
