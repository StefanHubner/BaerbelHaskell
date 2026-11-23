module Baerbel.ProviderConfig where

import Baerbel.JSON (jsonOptions)
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

data HttpPolicy = HttpPolicy {httpResponseBytes :: Int, httpTimeoutSeconds :: Int} deriving (Generic)
instance FromJSON HttpPolicy where parseJSON = genericParseJSON (jsonOptions 4)

data GmailRules = GmailRules
  { gmailHttp :: HttpPolicy, gmailBase :: Text
  , gmailBodyChars :: Int
  , gmailPartBytes :: Int
  , gmailThreadMessages :: Int
  , gmailThreadBytes :: Int
  , gmailMaximumParts :: Int
  , gmailMaximumDepth :: Int
  , gmailSearchLimit :: Int
  , gmailSearchConcurrency :: Int
  , gmailSenderChars :: Int
  , gmailRecipientChars :: Int
  , gmailSubjectChars :: Int
  , gmailDateChars :: Int
  , gmailSnippetChars :: Int
  , gmailAttachmentNameChars :: Int
  , gmailMimeChars :: Int
  , gmailSearchScope :: Text
  , gmailThreadScope :: Text
  , gmailHeaderChars :: Int, gmailSendBodyChars :: Int, gmailSendSubjectChars :: Int
  , gmailSearchChars :: Int, gmailPageTokenChars :: Int, gmailSendRecipients :: Int
  , gmailThreadHeaderChars :: Int, gmailReferenceCount :: Int, gmailEmptySubject :: Text
  , gmailMetadataHeaders :: [Text] } deriving (Generic)
instance FromJSON GmailRules where parseJSON = genericParseJSON (jsonOptions 5)

data ContactsRules = ContactsRules
  { contactsHttp :: HttpPolicy, contactsEndpoint :: Text
  , contactsNameCount :: Int
  , contactsPhoneCount :: Int
  , contactsNameChars :: Int
  , contactsNumberChars :: Int
  , contactsLabelChars :: Int
  , contactsQueryChars :: Int
  , contactsSearchLimit :: Int
  , contactsWarmupSeconds :: Int
  , contactsSearchScope :: Text } deriving (Generic)
instance FromJSON ContactsRules where parseJSON = genericParseJSON (jsonOptions 8)

data ProfileRules = ProfileRules
  { profileKeyChars :: Int
  , profileValueChars :: Int
  , profileEntryLimit :: Int
  , profileHomeLocationRole :: Text } deriving (Generic)
instance FromJSON ProfileRules where parseJSON = genericParseJSON (jsonOptions 7)

data CalendarRules = CalendarRules
  { calendarZoneChars :: Int, calendarHttp :: HttpPolicy, calendarZone :: Text
  , calendarCalendarUrl :: Text
  , calendarEventsUrl :: Text
  , calendarTitleChars :: Int
  , calendarLocationChars :: Int
  , calendarIdentifierChars :: Int
  , calendarListPages :: Int
  , calendarCalendarLimit :: Int
  , calendarListPageSize :: Int
  , calendarEventPages :: Int
  , calendarEventScanLimit :: Int
  , calendarEventPageSize :: Int
  , calendarMaxAttendees :: Int
  , calendarCalendarsChecked :: Int
  , calendarConcurrency :: Int
  , calendarOutputCount :: Int
  , calendarOutputBytes :: Int
  , calendarConflictCount :: Int
  , calendarReadableRoles :: [Text]
  , calendarUntitled :: Text
  , calendarPrimaryName :: Text
  , calendarUnnamed :: Text, calendarTokenChars :: Int, calendarIdChars :: Int
  , calendarMetadataBytes :: Int, calendarListFields :: Text, calendarEventFields :: Text
  , calendarAttendeeLimit :: Int, calendarCreateTitleChars :: Int, calendarDescriptionChars :: Int
  , calendarCreateLocationChars :: Int, calendarFilterChars :: Int } deriving (Generic)
instance FromJSON CalendarRules where parseJSON = genericParseJSON (jsonOptions 8)


data MapsRules = MapsRules
  { mapsApiStatuses :: [Text], mapsGeocodeUrl :: Text
  , mapsPlacesUrl :: Text
  , mapsDetailsUrl :: Text
  , mapsLanguage :: Text
  , mapsGeocodeRegion :: Text
  , mapsPlacesRegion :: Text
  , mapsBiasRadius :: Int
  , mapsNameChars :: Int
  , mapsAddressChars :: Int
  , mapsCandidateLimit :: Int
  , mapsReferenceBytes :: Int
  , mapsReferenceLimit :: Int
  , mapsHttpBytes :: Int
  , mapsHttpTimeout :: Int
  , mapsSearchFields :: Text
  , mapsDetailsFields :: Text
  , mapsApproximateTypes :: [Text]
  , mapsAmbiguousAreas :: [Text]
  , mapsApproximateWarning :: Text
  , mapsWeekdayLimit :: Int, mapsPeriodLimit :: Int, mapsSpecialDayLimit :: Int
  , mapsHoursDescriptionChars :: Int, mapsOffsetLimit :: Int, mapsPhoneChars :: Int
  , mapsZoneChars :: Int, mapsBusinessStatuses :: [Text]
  , mapsIdentifierChars :: Int, mapsPrecisions :: [Text], mapsAreaLimit :: Int
  , mapsLocationPrompt :: Text, mapsAreaPrompt :: Text, mapsQueryChars :: Int, mapsAreaChars :: Int } deriving (Generic)
instance FromJSON MapsRules where parseJSON = genericParseJSON (jsonOptions 4)

data RouteRules = RouteRules
  { routeRouteUrl :: Text
  , routeElevationUrl :: Text
  , routeLanguage :: Text
  , routeHttpBytes :: Int
  , routeHttpTimeout :: Int
  , routeElevationTimeout :: Int
  , routeGeometryLimit :: Int
  , routeStepLimit :: Int
  , routeInstructionChars :: Int
  , routeGeometryToleranceM :: Double
  , routeHeadingMinimumM :: Double
  , routeDurationLimitS :: Double
  , routeTerrainMinimumM :: Double
  , routeMaximumSpacingM :: Double
  , routeTargetSpacingM :: Double
  , routeSampleLimit :: Int
  , routeSampleMinimum :: Int
  , routeElevationLocationToleranceM :: Double
  , routeElevationMinimumResolutionM :: Double
  , routeElevationNoiseM :: Double
  , routeCompassNote :: Text
  , routeTerrainLimitations :: Text
  , routeRouteFields :: Text
  , routeDistanceLimitM :: Double, routeManeuverChars :: Int, routeWarningLimit :: Int
  , routeLabelChars :: Int, routeOriginLabel :: Text, routeDestinationLabel :: Text
  , routePlaceIdChars :: Int, routeOutputBytes :: Int, routeElevationMinimumM :: Double, routeElevationMaximumM :: Double
  , routeElevationMaximumResolutionM :: Double, routeElevationStatuses :: [Text], routeStepCoverage :: Double } deriving (Generic)
instance FromJSON RouteRules where parseJSON = genericParseJSON (jsonOptions 5)

data WindguruRules = WindguruRules
  { windguruFields :: [(Text, Text, Double, Double)], windguruUnits :: Map Text Text
  , windguruRequiredFields :: [Text], windguruMinimumTimestamp :: Double, windguruMaximumTimestamp :: Double
  , windguruMaximumSourceHours :: Double, windguruResolutionLimit :: Double, windguruStartChars :: Int
  , windguruProviderRows :: Int, windguruBaseUrl :: Text
  , windguruApiUrl :: Text
  , windguruHttpBytes :: Int
  , windguruHttpTimeout :: Int
  , windguruUserAgent :: Text
  , windguruCacheEntries :: Int
  , windguruCacheSeconds :: Double
  , windguruMaximumPoints :: Int
  , windguruMaximumSourcePoints :: Int
  , windguruMaximumSetPairs :: Int
  , windguruSetConcurrency :: Int
  , windguruFutureToleranceSeconds :: Double
  , windguruCountryLimit :: Int
  , windguruNameChars :: Int
  , windguruCountryChars :: Int
  , windguruModelChars :: Int
  , windguruSearchScope :: Text
  , windguruSunSource :: Text
  , windguruTideReason :: Text
  , windguruMetadataTideReason :: Text
  , windguruWindDirectionConvention :: Text
  , windguruWaveDirectionConvention :: Text
  , windguruModelInterpretation :: Text, windguruComparisonColumns :: [Text], windguruOmittedColumns :: [Text]
  , windguruComparisonBytes :: Int, windguruResultBytes :: Int, windguruDetailHint :: Text
  , windguruQueryChars :: Int, windguruSearchLimit :: Int, windguruSpotLimit :: Int, windguruModelLimit :: Int } deriving (Generic)
instance FromJSON WindguruRules where parseJSON = genericParseJSON (jsonOptions 8)
