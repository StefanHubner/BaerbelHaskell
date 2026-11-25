module Baerbel.Core.Accounts where

import Baerbel.Core.Config (Global(..), Settings, User(..), resolveSettings, setting)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (camelOptions)
import Baerbel.Core.Pin (parsePinHash)
import Baerbel.Core.Routing (normalizeDestination)
import Control.Monad (unless, void)
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Plan = Standard | Pro deriving (Eq, Generic)
instance FromJSON Plan
instance ToJSON Plan

data Account = Account
  { accountPlan :: Plan, accountName :: Text, accountPhone :: Text, accountEnvironment :: Map Text Text }
  deriving (Generic)
instance FromJSON Account where parseJSON = genericParseJSON (camelOptions 7)
instance ToJSON Account where toJSON = genericToJSON (camelOptions 7)

data Installation = Installation
  { installationEnvironment :: Map Text Text, installationAccounts :: Maybe (Map Text Account) }
  deriving (Generic)
instance FromJSON Installation where parseJSON = genericParseJSON (camelOptions 12)
instance ToJSON Installation where toJSON = genericToJSON (camelOptions 12)

data Subscriber = Subscriber { subscriberAccount :: Account, subscriberSettings :: Settings }

accountSettings :: Global -> Installation -> Result (Map Text Subscriber)
accountSettings global installation = do
  unless (separatePhones && length (filter ((== Pro) . accountPlan) entries) <= 1) invalid
  Map.traverseWithKey prepare accounts
  where
    accounts = fromMaybe Map.empty (installationAccounts installation)
    entries = Map.elems accounts
    separatePhones = not (null entries) && unique (map accountPhone entries)
    shared = Map.filterWithKey (\key _ -> key `elem` infrastructure) (installationEnvironment installation)
    prepare name account = do
      unless (validId name && normalizeDestination (accountPhone account) == Right (accountPhone account)) invalid
      let values = Map.union shared (Map.filterWithKey private (accountEnvironment account))
          identity = Map.fromList [("OWNER_MOBILE_E164", accountPhone account), ("INCOMING_OWNER_NAME", accountName account)]
          scoped = Map.union identity values
          settings = resolveSettings (namedDefaults (accountName account) global) (User (planSettings (accountPlan account) scoped))
      void (parsePinHash (setting settings "PHONE_ASSISTANT_PRIVATE_PIN_HASH"))
      Right (Subscriber account settings)
    private key _ = key `elem` personalKeys
    invalid = Left (failure "invalid_accounts")

namedDefaults :: Text -> Global -> Global
namedDefaults name global = global {globalDefaults = foldr personalize (globalDefaults global) keys}
  where
    keys = ["PHONE_ASSISTANT_GREETING", "PHONE_ASSISTANT_LIVE_INSTRUCTIONS"]
    personalize = Map.adjust (T.replace "Stefan" name)

infrastructure :: [Text]
infrastructure = ["OPENAI_API_KEY", "GOOGLE_MAPS_API_KEY", "PHONE_ASSISTANT_AMI_USERNAME"
  , "PHONE_ASSISTANT_AMI_PASSWORD", "PHONE_ASSISTANT_SMS_USERNAME", "PHONE_ASSISTANT_SMS_PASSWORD"
  , "PHONE_ASSISTANT_LOG_FILE"]

personalKeys :: [Text]
personalKeys = ["PHONE_ASSISTANT_PROFILE_FILE", "PHONE_ASSISTANT_GMAIL_TOKEN_FILE", "PHONE_ASSISTANT_GMAIL_STATE_FILE"
  , "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE", "PHONE_ASSISTANT_CALENDAR_STATE_FILE", "PHONE_ASSISTANT_CONTACTS_TOKEN_FILE"
  , "PHONE_ASSISTANT_WINDGURU", "PHONE_ASSISTANT_PRIVATE_PIN_HASH"
  , "PHONE_ASSISTANT_GREETING", "PHONE_ASSISTANT_DIRECT_CALLS", "PHONE_ASSISTANT_OWNER_NOTES"
  , "HOME_PHONE_ENABLED", "TELEMATICA_OUTBOUND_ENABLED", "INCOMING_MESSAGES_ENABLED"
  , "INCOMING_SMS_ENABLED", "PHONE_ASSISTANT_SMS_STATE_FILE", "INCOMING_EVENTS_FILE", "INCOMING_EVENTS_STATE_FILE"]

planSettings :: Plan -> Map Text Text -> Map Text Text
planSettings Pro = Map.insert "INCOMING_SCREENING_ENABLED" "true" . Map.insert "PHONE_ASSISTANT_DIRECT_CALLS" "true"
planSettings Standard = Map.union disabled
  where
    disabled = Map.fromList [(key, "false") | key <- ["INCOMING_SCREENING_ENABLED", "INCOMING_MESSAGES_ENABLED"
      , "INCOMING_SMS_ENABLED", "HOME_PHONE_ENABLED", "TELEMATICA_OUTBOUND_ENABLED"]]

validId :: Text -> Bool
validId name = name /= "shared" && not (T.null name) && T.length name <= 40 && T.all allowed name
  where allowed char = char `elem` ("abcdefghijklmnopqrstuvwxyz0123456789-" :: String)

unique :: Eq a => [a] -> Bool
unique values = length (nub values) == length values

accountForPhone :: Map Text Subscriber -> Text -> Maybe (Text, Subscriber)
accountForPhone accounts phone = Map.lookup canonical index
  where
    canonical = case T.stripPrefix "00" phone of
      Just digits -> "+" <> digits
      Nothing | T.isPrefixOf "44" phone -> "+" <> phone
      Nothing -> maybe phone ("+44" <>) (T.stripPrefix "0" phone)
    index = Map.fromList [(accountPhone (subscriberAccount user), (ident, user)) | (ident, user) <- Map.toList accounts]

accountStateKeys :: [Text]
accountStateKeys = ["PHONE_ASSISTANT_PROFILE_FILE", "PHONE_ASSISTANT_GMAIL_TOKEN_FILE", "PHONE_ASSISTANT_GMAIL_STATE_FILE"
  , "PHONE_ASSISTANT_CALENDAR_TOKEN_FILE", "PHONE_ASSISTANT_CALENDAR_STATE_FILE", "PHONE_ASSISTANT_CONTACTS_TOKEN_FILE"
  , "PHONE_ASSISTANT_SMS_STATE_FILE", "INCOMING_EVENTS_FILE", "INCOMING_EVENTS_STATE_FILE"]
