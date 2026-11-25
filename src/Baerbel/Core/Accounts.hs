module Baerbel.Core.Accounts where

import Baerbel.Core.Config (Global, Settings, User(..), booleanSetting, resolveSettings, setting)
import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.Pin (parsePinHash)
import Baerbel.Core.Routing (normalizeDestination)
import Control.Monad (unless, void)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data Plan = Standard | Pro deriving (Eq)

data Account = Account
  { accountPlan :: Plan, accountName :: Text, accountPhone :: Text, accountEnvironment :: Map Text Text }

data Subscriber = Subscriber { subscriberAccount :: Account, subscriberSettings :: Settings }

accountSettings :: Global -> Map Text Text -> Map Text Account -> Result (Map Text Subscriber)
accountSettings global shared accounts = do
  unless separatePhones invalid
  checkKeys "shared" infrastructure shared
  subscribers <- Map.traverseWithKey prepare accounts
  let active key = filter (enabled key) (Map.elems subscribers)
      sharedResource key = length (active key) <= 1
  unless (all sharedResource ["HOME_PHONE_ENABLED", "INCOMING_SMS_ENABLED"]) overlap
  Right subscribers
  where
    entries = Map.elems accounts
    separatePhones = not (null entries) && unique (map accountPhone entries)
    prepare name account = do
      unless (validId name && normalizeDestination (accountPhone account) == Right (accountPhone account)) invalid
      let plan = accountPlan account
          private = accountEnvironment account
      checkKeys name (personalKeys plan) private
      let values = Map.union shared private
          identity = Map.fromList [("OWNER_MOBILE_E164", accountPhone account), ("INCOMING_OWNER_NAME", accountName account)]
          scoped = Map.union identity values
          settings = resolveSettings global (User (planSettings plan scoped))
      void (parsePinHash (setting settings "PHONE_ASSISTANT_PRIVATE_PIN_HASH"))
      Right (Subscriber account settings)
    enabled key sub = booleanSetting (subscriberSettings sub) key == Right True
    overlap = Left (failure "account_resources_overlap")
    invalid = Left (failure "invalid_accounts")

checkKeys :: Text -> [Text] -> Map Text Text -> Result ()
checkKeys scope allowed values = case filter (`notElem` allowed) (Map.keys values) of
  [] -> Right ()
  keys -> Left (failure ("unsupported_account_settings:" <> scope <> ":" <> T.intercalate "," keys))

infrastructure :: [Text]
infrastructure = ["OPENAI_API_KEY", "GOOGLE_MAPS_API_KEY", "PHONE_ASSISTANT_AMI_USERNAME"
  , "PHONE_ASSISTANT_AMI_PASSWORD", "PHONE_ASSISTANT_SMS_USERNAME", "PHONE_ASSISTANT_SMS_PASSWORD"
  , "PHONE_ASSISTANT_LOG_FILE"]

personalKeys :: Plan -> [Text]
personalKeys plan = planKeys plan ++
  ["PHONE_ASSISTANT_WINDGURU", "PHONE_ASSISTANT_PRIVATE_PIN_HASH", "PHONE_ASSISTANT_GREETING", "PHONE_ASSISTANT_OWNER_NOTES"]
  where
    planKeys :: Plan -> [Text]
    planKeys Standard = ["PHONE_ASSISTANT_DIRECT_CALLS"]
    planKeys Pro = "INCOMING_EVENTS_FILE" : proSwitches

proSwitches :: [Text]
proSwitches = ["HOME_PHONE_ENABLED", "TELEMATICA_OUTBOUND_ENABLED", "INCOMING_MESSAGES_ENABLED", "INCOMING_SMS_ENABLED"]

planSettings :: Plan -> Map Text Text -> Map Text Text
planSettings Pro = Map.insert "INCOMING_SCREENING_ENABLED" "true" . Map.insert "PHONE_ASSISTANT_DIRECT_CALLS" "true"
planSettings Standard = Map.union disabled
  where
    disabled = Map.fromList [(key, "false") | key <- "INCOMING_SCREENING_ENABLED" : proSwitches]

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
