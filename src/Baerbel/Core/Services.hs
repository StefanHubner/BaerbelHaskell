module Baerbel.Core.Services where

import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Core.HTTP ( Request(..) )
import Baerbel.Core.Profile ( ProfileState(..), snapshot )
import Baerbel.Core.ProviderConfig ( ProfileRules )
import Data.Aeson
    ( toJSON, FromJSON(..), ToJSON, genericParseJSON, genericToJSON )
import Data.List ( find )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Baerbel.Core.JSON ( jsonOptions, encodeReference )
import qualified Data.Map.Strict as Map ( lookup )
import qualified Data.Text as Text ( isPrefixOf )
import qualified Data.Text.Encoding as Text ( encodeUtf8 )

data AccountSpec = AccountSpec Text Text Text

accounts :: [AccountSpec]
accounts = [AccountSpec "gmail" "PHONE_ASSISTANT_GMAIL_TOKEN_FILE" "gmail",
            AccountSpec "calendar" "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE" "calendar",
            AccountSpec "contacts" "PHONE_ASSISTANT_CONTACTS_TOKEN_FILE" "contacts"]

data Mutation = Mutation
  { mutationMethod :: Text, mutationUrl :: Text, mutationDefiniteMinimum :: Int
  , mutationDefiniteMaximum :: Int, mutationUncertainStatuses :: [Int] } deriving (Generic)
instance FromJSON Mutation where parseJSON = genericParseJSON (Baerbel.Core.JSON.jsonOptions 8)

mutationPolicy :: [Mutation] -> Request -> Maybe (Int -> Bool)
mutationPolicy mutations request = uncertainStatus <$> find matches mutations
  where
  uncertainStatus mutation status = status < mutationDefiniteMinimum mutation
    || status > mutationDefiniteMaximum mutation || status `elem` mutationUncertainStatuses mutation
  matches mutation = Text.encodeUtf8 (mutationMethod mutation) == requestMethod request && mutationUrl mutation == requestUrl request

prefixErrors :: Text -> Result a -> Result a
prefixErrors prefix = either (Left . addPrefix) Right
  where
  addPrefix err = err {failureCode = prefix <> "_" <> failureCode err}

mapsAuthorization :: Text -> Request -> Request
mapsAuthorization key request
  | requestMethod request == "POST" || places = request {requestHeaders = header : requestHeaders request}
  | otherwise = request {requestQuery = parameter : requestQuery request}
  where
  places = "https://places.googleapis.com/v1/places/" `Text.isPrefixOf` requestUrl request
  encoded = Text.encodeUtf8 key
  header = ("X-Goog-Api-Key", encoded)
  parameter = ("key", Just encoded)

readForecastSet :: Result (Maybe ProfileState) -> Result Text
readForecastSet result = result >>= maybe missing select
  where
    select state = maybe missing Right (Map.lookup "windguru_forecast_set" (profileValues state))
    missing = Left (failure "windguru_forecast_set_not_configured")

renderProfileContext :: ProfileRules -> Text -> Result ProfileState -> Text
renderProfileContext rules prefix result = prefix <> Baerbel.Core.JSON.encodeReference value
  where value = either (toJSON . unavailableProfile) (\state -> snapshot rules state Nothing) result

data UnavailableProfile = UnavailableProfile
  { unavailableStatus :: Text, unavailableError :: Text, unavailableProfileState :: Text } deriving (Generic)
instance ToJSON UnavailableProfile where toJSON = genericToJSON (Baerbel.Core.JSON.jsonOptions 11)

unavailableProfile :: Failure -> UnavailableProfile
unavailableProfile err = UnavailableProfile "error" (failureCode err) "unavailable"
