module Baerbel.Core.Asterisk
  ( Pbx(..), Sip(..), Roster(..), capture, render, managedName, includePaths ) where

import Baerbel.Core.Error (Result, failure)
import Baerbel.Core.JSON (camelOptions)
import Data.Aeson (FromJSON(..), Options(rejectUnknownFields), ToJSON(..), genericParseJSON, genericToJSON)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Pbx = Pbx
  { pbxDirectory :: FilePath, pbxPrimaryPro :: Text, pbxFiles :: Map FilePath Text }
  deriving (Generic)
instance FromJSON Pbx where parseJSON = genericParseJSON ((camelOptions 3) {rejectUnknownFields = True})
instance ToJSON Pbx where toJSON = genericToJSON (camelOptions 3)

data Sip = Sip {sipNumber :: Text, sipUser :: Text, sipPassword :: Text}
data Roster = Roster {standardSip :: Sip, proSip :: Map Text (Text, Sip)}

managedName :: FilePath -> Maybe FilePath
managedName name = stripPrefix "phone-assistant-" name >>= valid
  where
    valid path | not (null path) && all allowed path = Just path
               | otherwise = Nothing
    allowed char = char `elem` ("abcdefghijklmnopqrstuvwxyz0123456789-." :: String)

capture :: FilePath -> Text -> Text -> Text -> Text -> Map FilePath Text -> Result (Pbx, Sip, Sip)
capture root previous primary shared dedicated files = do
  outbound <- required "outbound.conf" files
  bristol <- required "bristol-pjsip.conf" files
  standard <- credentials shared "phone-assistant-aa-outbound-auth" outbound
  pro <- credentials dedicated "phone-assistant-aa-bristol-auth" bristol
  let mainFiles = Map.map (markRoutes previous shared dedicated) files
      managed = Map.insert "outbound.conf" (templateAuth "phone-assistant-aa-outbound-auth" "SHARED" (markRoutes previous shared dedicated outbound)) mainFiles
      templated = Map.insert "bristol-pjsip.conf" (templateAuth "phone-assistant-aa-bristol-auth" "PRO" (markProNumber pro bristol)) managed
  pure (Pbx root primary templated, standard, pro)

render :: Pbx -> Roster -> Result (Map FilePath Text)
render pbx roster = do
  (_, primary) <- required (pbxPrimaryPro pbx) (proSip roster)
  source <- required "bristol-pjsip.conf" (pbxFiles pbx)
  dialplan <- required "extensions.conf" (pbxFiles pbx)
  let shared = standardSip roster
      replacements = sipFields "SHARED" shared ++ sipFields "PRO" primary ++ [("@@PRIMARY_PRO@@", pbxPrimaryPro pbx)]
      flags = Map.delete (pbxPrimaryPro pbx) (proSip roster)
      prepared = Map.map (replaceAll replacements) (pbxFiles pbx)
      additional = Map.mapWithKey (extraSip source) flags
      sip = replaceAll replacements source <> Text.concat (Map.elems additional)
      routes = extendRoutes dialplan flags
      filled = Map.insert "extensions.conf" (replaceAll replacements routes) prepared
      files = Map.insert "bristol-pjsip.conf" sip filled
  case all validFile (Map.toList files) of
    True -> Right files
    False -> Left (failure "invalid_asterisk_templates")

validFile :: (FilePath, Text) -> Bool
validFile (path, content) = managedName ("phone-assistant-" <> path) == Just path
  && path /= "." && path /= ".." && not ("@@" `Text.isInfixOf` content)

required :: (Ord k) => k -> Map k a -> Result a
required key = maybe (Left (failure "asterisk_setting_required")) Right . Map.lookup key

credentials :: Text -> Text -> Text -> Result Sip
credentials number section source =
  Sip number <$> required "username" values <*> required "password" values
  where
    rows = drop 1 (dropWhile ((/= ("[" <> section <> "]")) . Text.strip) (Text.lines source))
    values = Map.fromList (map field (takeWhile (not . sectionLine) rows))
    field line = let (key, value) = Text.breakOn "=" line in (Text.strip key, Text.strip (Text.drop 1 value))

sectionLine :: Text -> Bool
sectionLine = Text.isPrefixOf "[" . Text.strip

templateAuth :: Text -> Text -> Text -> Text
templateAuth section tag source = Text.unlines (before ++ map mark body ++ after)
  where
    (start, following) = break ((== ("[" <> section <> "]")) . Text.strip) (Text.lines source)
    (header, rest) = splitAt 1 following
    before = start ++ header
    (body, after) = break sectionLine rest
    mark line = case Text.strip (Text.takeWhile (/= '=') line) of
      "username" -> "username=@@" <> tag <> "_USER@@"
      "password" -> "password=@@" <> tag <> "_PASSWORD@@"
      _ -> line

markRoutes :: Text -> Text -> Text -> Text -> Text
markRoutes previous shared dedicated = replaceAll
  [ ("PA_ACCOUNT=" <> previous <> ")", "PA_ACCOUNT=@@PRIMARY_PRO@@)")
  , ("PA_ENTRY=" <> previous <> ")", "PA_ENTRY=@@PRIMARY_PRO@@)")
  , (shared, "@@SHARED_DID@@"), (Text.drop 1 shared, "@@SHARED_DIGITS@@")
  , (dedicated, "@@PRO_DID@@"), (Text.drop 1 dedicated, "@@PRO_DIGITS@@") ]

markProNumber :: Sip -> Text -> Text
markProNumber pro = Text.replace (Text.drop 3 (sipNumber pro)) "@@PRO_LOCAL@@"

sipFields :: Text -> Sip -> [(Text, Text)]
sipFields tag sip = map field fields
  where
    number = sipNumber sip
    fields = [("DID", number), ("DIGITS", Text.drop 1 number), ("LOCAL", Text.drop 3 number)
      , ("USER", sipUser sip), ("PASSWORD", sipPassword sip)]
    field (key, value) = ("@@" <> tag <> "_" <> key <> "@@", value)

replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll fields source = foldl' replace source fields
  where replace content (key, value) = Text.replace key value content

extraSip :: Text -> Text -> (Text, Sip) -> Text
extraSip source ident (_, sip) = "\n" <> replaceAll fields source
  where
    endpoint = "phone-assistant-aa-pro-" <> ident
    fields = ("phone-assistant-aa-bristol", endpoint) : sipFields "PRO" sip

extendRoutes :: Text -> Map Text (Text, Sip) -> Text
extendRoutes source extra
  | Map.null extra = source
  | otherwise = Text.replace original (preface <> matches <> Text.replace "exten => s,1," " same => n," original) source <> contexts
  where
    original = "exten => s,1,GotoIf($[\"${CHANNEL(endpoint)}\"=\"phone-assistant-aa-bristol\"]?dedicated)"
    preface = "exten => s,1,NoOp(Baerbel SIP account)\n"
    matches = Text.concat (map select (Map.keys extra))
    select ident = " same => n,GotoIf($[\"${CHANNEL(endpoint)}\"=\"phone-assistant-aa-pro-" <> ident <> "\"]?baerbel-pro-" <> ident <> ",s,1)\n"
    contexts = Text.concat (map context (Map.keys extra))
    context ident = "\n[baerbel-pro-" <> ident <> "]\nexten => s,1,Set(PA_ENTRY=" <> ident <> ")\n"
      <> " same => n,Set(PA_TRACE_ROUTE=pro-" <> ident <> ")\n same => n,Goto(baerbel-account-entry,s,1)\n"

includePaths :: [FilePath] -> Text -> Text
includePaths paths = replaceAll (map relocation paths)
  where relocation name = ("phone-assistant-" <> Text.pack name, "phone-assistant/" <> Text.pack name)
