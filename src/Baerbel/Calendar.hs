{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Baerbel.Calendar (calendar, TimeZones(..), eventSummary) where

import Baerbel.TimeFormat (isoZoned)
import Data.Char (isDigit)
import qualified Data.Bifunctor as Bifunctor
import Baerbel.Error (Failure(..), Result, failure)
import Baerbel.HTTP (Send)
import Baerbel.GoogleValidation (address, textValue)
import Baerbel.JSON (decodeValue, argumentOptions, camelOptions, jsonOptions)
import Baerbel.Provider
import Baerbel.ProviderConfig (CalendarRules(..))
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
import Data.List (nub, sortOn, mapAccumL)
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Types.URI (urlEncode)

data TimeZones = TimeZones
  { localInstant :: Text -> LocalTime -> Result UTCTime
  , zonedInstant :: Text -> UTCTime -> Result ZonedTime }

data CalendarEntry = CalendarEntry
  { entryId :: Text, entrySummary :: Maybe Text, entrySummaryOverride :: Maybe Text
  , entryAccessRole :: Maybe Text, entryPrimary :: Maybe Bool, entryDeleted :: Maybe Bool
  , entryTimeZone :: Maybe Text } deriving (Generic)
instance FromJSON CalendarEntry where
  parseJSON = withObject "calendar metadata" (\value -> CalendarEntry <$> value .:? "id" .!= ""
    <*> value .:? "summary" <*> value .:? "summaryOverride" <*> pure (optionalField "accessRole" value) <*> pure (optionalField "primary" value)
    <*> pure (optionalField "deleted" value) <*> pure (optionalField "timeZone" value))
data CalendarPage = CalendarPage {listItems :: Maybe [Value], listNextPageToken :: Maybe Text} deriving (Generic)
instance FromJSON CalendarPage where
  parseJSON = withObject "calendar page" (\value -> CalendarPage <$> value .:! "items" <*> value .:? "nextPageToken")
data EventTime = EventTime {whenDate :: Maybe Text, whenDateTime :: Maybe Text, whenTimeZone :: Maybe Text} deriving (Generic)
instance FromJSON EventTime where parseJSON = genericParseJSON (camelOptions 4)
instance ToJSON EventTime where toJSON = genericToJSON ((camelOptions 4) {omitNothingFields = True})
data Attendee = Attendee {attendeeSelf :: Maybe Bool, attendeeResponseStatus :: Maybe Text} deriving (Generic)
instance FromJSON Attendee where parseJSON = genericParseJSON (camelOptions 8)
data Event = Event
  { eventId :: Maybe Text, eventSummaryText :: Maybe Text, eventStatus :: Maybe Text
  , eventLocation :: Maybe Text, eventStart :: Maybe EventTime, eventEnd :: Maybe EventTime
  , eventTransparency :: Maybe Text, eventAttendees :: Maybe [Attendee] } deriving (Generic)
instance FromJSON Event where parseJSON = withObject "Calendar event" parseEvent
parseEvent :: Object -> Parser Event
parseEvent value = Event <$> value .:? "id" <*> value .:? "summary" <*> value .:? "status"
  <*> value .:? "location" <*> value .:? "start" <*> value .:? "end"
  <*> value .:? "transparency" <*> value .:? "attendees"
data EventPage = EventPage {eventsItems :: Maybe [Value], eventsNextPageToken :: Maybe Text} deriving (Generic)
instance FromJSON EventPage where
  parseJSON = withObject "event page" (\value -> EventPage <$> value .:! "items" <*> value .:? "nextPageToken")

newtype AgendaArgs
  = AgendaArgs {agendaDate :: Text}
  deriving (Generic)
instance FromJSON AgendaArgs where parseJSON = genericParseJSON (argumentOptions 6)
data SearchArgs = SearchArgs
  { searchStartDate :: Text, searchEndDate :: Text, searchQuery :: Text, searchCalendar :: Text } deriving (Generic)
instance FromJSON SearchArgs where parseJSON = genericParseJSON (argumentOptions 6)
data CreateArgs = CreateArgs
  { createSummary :: Text, createStart :: Text, createEnd :: Text, createDescription :: Text
  , createLocation :: Text, createAttendees :: [Text], createAllowConflicts :: Bool } deriving (Generic)
instance FromJSON CreateArgs where parseJSON = genericParseJSON (argumentOptions 6)
instance ToJSON CreateArgs where toJSON = genericToJSON (jsonOptions 6)

data EventSummary = EventSummary
  { sumCalendarId :: Text, sumCalendarName :: Text, sumTimeZone :: Text, sumEventId :: Text
  , sumSummary :: Text, sumStart :: Text, sumEnd :: Text, sumAllDay :: Bool
  , sumLocation :: Text, sumBusy :: Bool } deriving (Eq, Show, Generic)
instance ToJSON EventSummary where toJSON = genericToJSON (jsonOptions 3)
type CalendarEvent = (UTCTime, EventSummary)
data CalendarRow = CalendarRow
  { rowCalendarId :: Text, rowCalendarName :: Text, rowTimeZone :: Nullable Text
  , rowAccessRole :: Text, rowPrimary :: Bool, rowStatus :: Text, rowReason :: Maybe Text } deriving (Generic)
instance ToJSON CalendarRow where toJSON = genericToJSON ((jsonOptions 3) {omitNothingFields = True})
data Coverage = Coverage
  { coverageCoverageComplete :: Bool, coverageCalendarListComplete :: Bool
  , coverageCalendarListError :: Nullable Text, coverageCalendarListTruncated :: Bool
  , coverageCalendarsFound :: Int, coverageCalendarsChecked :: Int, coverageCalendarResults :: [CalendarRow]
  , coverageCalendarSelectionError :: Maybe Text } deriving (Generic)
instance ToJSON Coverage where toJSON = genericToJSON ((jsonOptions 8) {omitNothingFields = True})
data Agenda = Agenda
  { outputStatus :: Text, outputDate :: Maybe Text, outputStartDate :: Maybe Text, outputEndDate :: Maybe Text
  , outputQuery :: Maybe Text, outputCalendar :: Maybe Text, outputTimeZone :: Text
  , outputEvents :: [EventSummary], outputEventCount :: Int, outputHasMore :: Bool
  , outputTruncated :: Bool, outputEventOutputLimited :: Bool, outputUntrustedCalendarContent :: Bool
  , outputError :: Maybe Text } deriving (Generic)
instance ToJSON Agenda where toJSON = genericToJSON ((jsonOptions 6) {omitNothingFields = True})
newtype Email
  = Email {emailEmail :: Text}
  deriving (Generic)
instance ToJSON Email where toJSON = genericToJSON (camelOptions 5)
data EventResource = EventResource
  { resourceId :: Text, resourceSummary :: Text, resourceStart :: EventTime, resourceEnd :: EventTime
  , resourceDescription :: Text, resourceLocation :: Text, resourceAttendees :: [Email] } deriving (Generic)
instance ToJSON EventResource where toJSON = genericToJSON (camelOptions 8)
data Created = Created
  { createdStatus :: Text, createdCreated :: Bool, createdEvent :: EventSummary, createdAttendees :: [Text]
  , createdInvitationsRequested :: Bool, createdRetrySafe :: Bool } deriving (Generic)
instance ToJSON Created where toJSON = genericToJSON (jsonOptions 7)
data Conflict = Conflict
  { conflictStatus :: Text, conflictError :: Text, conflictRetrySafe :: Bool, conflictConflicts :: [EventSummary]
  , conflictConflictsTruncated :: Bool, conflictUntrustedCalendarContent :: Bool } deriving (Generic)
instance ToJSON Conflict where toJSON = genericToJSON (jsonOptions 8)

eventUrl :: CalendarRules -> Text -> Text
eventUrl rules ident = calendarEventsUrl rules <> encoded <> "/events"
  where encoded = Text.decodeUtf8 (urlEncode True (Text.encodeUtf8 ident))

parseDay :: Text -> Result Day
parseDay text
  | validDaySyntax text = maybe invalid Right parsed
  | otherwise = invalid
  where
    invalid = Left (failure "invalid_calendar_date")
    parsed = parseTimeM True defaultTimeLocale "%F" (Text.unpack text)

validDaySyntax :: Text -> Bool
validDaySyntax value = map Text.length parts == [4, 2, 2] && all (Text.all asciiDigit) parts
  where parts = Text.splitOn "-" value

asciiDigit :: Char -> Bool
asciiDigit = isDigit

validTimestampSyntax :: Text -> Bool
validTimestampSyntax value = case Text.splitOn "T" value of
  [day, clock] -> validDaySyntax day && validClock clock
  _ -> False

validClock :: Text -> Bool
validClock supplied = validOffset && case Text.splitOn ":" time of
  [hours, minutes, seconds] -> number 23 hours && number 59 minutes && validSeconds seconds
  _ -> False
  where
    utcSuffix = "Z" `Text.isSuffixOf` supplied
    suffixLength | utcSuffix = 1
                 | otherwise = 6
    time = Text.dropEnd suffixLength supplied
    offset = Text.takeEnd 6 supplied
    validOffset = utcSuffix || (Text.take 1 offset `elem` ["+", "-"] && case Text.splitOn ":" (Text.drop 1 offset) of
      [hours, minutes] -> number 23 hours && number 59 minutes
      _ -> False)
    number maximumValue text = Text.length text == 2 && Text.all asciiDigit text && maybe False (<= maximumValue) (readMaybe (Text.unpack text) :: Maybe Int)
    validSeconds seconds = case Text.splitOn "." seconds of
      [whole] -> number 59 whole
      [whole, fraction] -> number 59 whole && Text.length fraction `elem` [3, 6] && Text.all asciiDigit fraction
      _ -> False
parseInstant :: Text -> Result UTCTime
parseInstant text
  | validTimestampSyntax text = maybe invalid (Right . zonedTimeToUTC) parsed
  | otherwise = invalid
  where
    invalid = Left (failure "invalid_calendar_timestamp")
    parsed = parseTimeM True defaultTimeLocale "%FT%T%Q%Ez" (Text.unpack normalized)
    normalized | "Z" `Text.isSuffixOf` text = Text.dropEnd 1 text <> "+00:00"
               | otherwise = text

bounds :: TimeZones -> Maybe Text -> Event -> Result (UTCTime, UTCTime, Bool)
bounds zones selected event = case (eventStart event, eventEnd event) of
  (Just start, Just end) -> parseBounds start end >>= ordered
  _ -> Left (failure "calendar_invalid_event")
  where
    parseBounds start end = case (whenDate start, whenDate end, whenDateTime start, whenDateTime end) of
      (Just first, Just lastDay, _, _) -> (,, True) <$> midnightFor first <*> midnightFor lastDay
      (_, _, Just first, Just lastTime) -> (,, False) <$> parseInstant first <*> parseInstant lastTime
      _ -> Left (failure "calendar_invalid_event")
    midnightFor dayText = do
      name <- maybe (Left (failure "calendar_unknown_time_zone")) Right selected
      day <- parseDay dayText
      either (const (Left (failure "calendar_unknown_time_zone"))) Right (localInstant zones name (LocalTime day midnight))
    ordered result@(first, lastTime, _)
      | lastTime <= first = Left (failure "calendar_invalid_event")
      | otherwise = Right result

visible :: Event -> Bool
visible event = eventStatus event /= Just "cancelled" && not declined
  where
    attendees = fromMaybe [] (eventAttendees event)
    declined = any (\item -> attendeeSelf item == Just True && attendeeResponseStatus item == Just "declined") attendees

eventSummary :: CalendarRules -> TimeZones -> CalendarEntry -> Event -> Result EventSummary
eventSummary rules zones entry event = bounds zones (entryTimeZone entry) event >>= summarizeEvent rules zones entry event

summarizeEvent :: CalendarRules -> TimeZones -> CalendarEntry -> Event -> (UTCTime, UTCTime, Bool) -> Result EventSummary
summarizeEvent rules zones entry event (start, end, allDay) = do
  first <- zonedInstant zones selected start
  lastTime <- zonedInstant zones selected end
  let beginning = format first
      finish = format lastTime
  Right (EventSummary (entryId entry) (entryName rules entry) selected ident title beginning finish allDay location busy)
  where
    selected | allDay = fromMaybe (calendarZone rules) (entryTimeZone entry)
             | otherwise = calendarZone rules
    format :: ZonedTime -> Text
    format | allDay = Text.pack . formatTime defaultTimeLocale "%F"
           | otherwise = isoZoned
    title = Text.take (calendarTitleChars rules) (nonempty (calendarUntitled rules) (eventSummaryText event))
    location = Text.take (calendarLocationChars rules) (fromMaybe "" (eventLocation event))
    ident = Text.take (calendarIdentifierChars rules) (fromMaybe "" (eventId event))
    busy = eventTransparency event /= Just "transparent"

nonempty :: Text -> Maybe Text -> Text
nonempty fallback Nothing = fallback
nonempty fallback (Just "") = fallback
nonempty _ (Just text) = text

entryName :: CalendarRules -> CalendarEntry -> Text
entryName rules entry = Text.take (calendarTitleChars rules) chosen
  where chosen = nonempty (nonempty (calendarUnnamed rules) (entrySummary entry)) (entrySummaryOverride entry)

data Discovery = Discovery {discoveredEntries :: [CalendarEntry], discoveredComplete :: Bool, discoveredError :: Maybe Text}

listCalendars :: CalendarRules -> Send -> IO Discovery
listCalendars rules send = loop 0 0 [] Set.empty [] Nothing
  where
    loop :: Int -> Int -> [CalendarEntry] -> Set.Set Text -> [Text] -> Maybe Text -> IO Discovery
    loop pages scanned found ids seen token
      | pages >= calendarListPages rules || scanned >= calendarCalendarLimit rules = pure (Discovery found False Nothing)
      | otherwise = requestPage pages scanned found ids seen token
    requestPage :: Int -> Int -> [CalendarEntry] -> Set.Set Text -> [Text] -> Maybe Text -> IO Discovery
    requestPage pages scanned found ids seen token = do
      response <- fetch "calendar_invalid_calendar_list" (get (calendarHttp rules) send (calendarCalendarUrl rules) (params token))
      either (pure . Discovery found False . Just . failureCode) (next pages scanned found ids seen) response
    params token = [textParam "showDeleted" "false", textParam "showHidden" "true", textParam "maxResults" (Text.pack (show (calendarListPageSize rules))), textParam "fields" (calendarListFields rules)]
      <> maybe [] (pure . textParam "pageToken") token
    next pages scanned found ids seen response = case collectCalendars rules scanned found ids (fromMaybe [] (listItems response)) of
      Left (issue, entries) -> pure (Discovery entries False (Just (failureCode issue)))
      Right (count, entries, known, cut) -> advance pages count entries known seen cut (listNextPageToken response)
    advance :: Int -> Int -> [CalendarEntry] -> Set.Set Text -> [Text] -> Bool -> Maybe Text -> IO Discovery
    advance _ _ entries _ _ True _ = pure (Discovery entries False Nothing)
    advance pages count entries known seen False token = case token of
      Nothing -> pure (Discovery entries True Nothing)
      Just "" -> pure (Discovery entries True Nothing)
      Just page | Text.length page > calendarTokenChars rules || page `elem` seen -> pure (Discovery entries False (Just "calendar_invalid_pagination"))
      Just page -> loop (pages + 1) count entries known (page : seen) (Just page)

data CalendarIdentity = CalendarIdentity
  { identityCalendarId :: Text, identityCalendarName :: Text, identityAccessRole :: Text
  , identityTimeZone :: Maybe Text, identityPrimary :: Bool } deriving (Generic)
instance ToJSON CalendarIdentity where toJSON = genericToJSON (jsonOptions 8)

calendarIdentity :: CalendarRules -> CalendarEntry -> CalendarIdentity
calendarIdentity rules entry = CalendarIdentity (entryId entry) (entryName rules entry)
  (fromMaybe "unknown" (entryAccessRole entry)) (entryTimeZone entry) (entryPrimary entry == Just True)

normalizeEntry :: CalendarRules -> CalendarEntry -> CalendarEntry
normalizeEntry rules entry = entry {entryAccessRole = Just role, entryTimeZone = zone}
  where
    supplied = fromMaybe "unknown" (entryAccessRole entry)
    role | supplied `elem` ("freeBusyReader" : calendarReadableRoles rules) = supplied
         | otherwise = "unknown"
    zone = entryTimeZone entry >>= boundedZone
    boundedZone :: Text -> Maybe Text
    boundedZone name
      | Text.length name <= calendarZoneChars rules = Just name
      | otherwise = Nothing

collectCalendars :: CalendarRules -> Int -> [CalendarEntry] -> Set.Set Text -> [Value] -> Either (Failure, [CalendarEntry]) (Int, [CalendarEntry], Set.Set Text, Bool)
collectCalendars _ scanned found ids [] = Right (scanned, found, ids, False)
collectCalendars rules scanned found ids (value : rest)
  | scanned >= calendarCalendarLimit rules = Right (scanned, found, ids, True)
  | otherwise = case decodeValue "calendar_invalid_calendar_list" value of
      Left issue -> Left (issue, found)
      Right entry -> collectCalendar rules scanned found ids rest (normalizeEntry rules entry)

collectCalendar :: CalendarRules -> Int -> [CalendarEntry] -> Set.Set Text -> [Value] -> CalendarEntry -> Either (Failure, [CalendarEntry]) (Int, [CalendarEntry], Set.Set Text, Bool)
collectCalendar rules scanned found ids rest entry
  | entryDeleted entry == Just True = next found ids
  | Text.null ident || Text.length ident > calendarIdChars rules || Text.any (< ' ') ident = Left (failure "calendar_invalid_calendar_list", found)
  | Set.member ident ids = next found ids
  | sum (map (referenceSize . calendarIdentity rules) (found <> [entry])) > calendarMetadataBytes rules = Right (scanned, found, ids, True)
  | otherwise = next (found <> [entry]) (Set.insert ident ids)
  where
    ident = entryId entry
    next entries known = collectCalendars rules (scanned + 1) entries known rest

listEvents :: CalendarRules -> Send -> TimeZones -> UTCTime -> UTCTime -> Text -> CalendarEntry -> IO (Result ([CalendarEvent], Bool))
listEvents rules send zones first lastTime query entry = loop 0 0 [] Set.empty [] Nothing
  where
    url = eventUrl rules (entryId entry)
    stamp = Text.pack . formatTime defaultTimeLocale "%FT%TZ"
    wireStart = UTCTime (utctDay first) (fromInteger (floor (utctDayTime first)))
    wireEnd = UTCTime (utctDay lastTime) (fromInteger (ceiling (utctDayTime lastTime)))
    params token = [textParam "timeMin" (stamp wireStart), textParam "timeMax" (stamp wireEnd), textParam "fields" (calendarEventFields rules),
      textParam "timeZone" (calendarZone rules), textParam "singleEvents" "true", textParam "orderBy" "startTime", textParam "showDeleted" "false", textParam "showHiddenInvitations" "true",
      textParam "maxResults" (Text.pack (show (calendarEventPageSize rules))), textParam "maxAttendees" (Text.pack (show (calendarMaxAttendees rules)))] <> optionalQuery <> maybe [] (pure . textParam "pageToken") token
    optionalQuery = [textParam "q" query | not (Text.null query)]
    loop :: Int -> Int -> [CalendarEvent] -> Set.Set Text -> [Text] -> Maybe Text -> IO (Result ([CalendarEvent], Bool))
    loop pages scanned events ids seen token
      | pages >= calendarEventPages rules || scanned >= calendarEventScanLimit rules = pure (Right (events, True))
      | otherwise = requestPage pages scanned events ids seen token
    requestPage :: Int -> Int -> [CalendarEvent] -> Set.Set Text -> [Text] -> Maybe Text -> IO (Result ([CalendarEvent], Bool))
    requestPage pages scanned events ids seen token = do
      response <- fetch "calendar_invalid_response" (get (calendarHttp rules) send url (params token))
      either (pure . Left) (next pages scanned events ids seen) response
    next :: Int -> Int -> [CalendarEvent] -> Set.Set Text -> [Text] -> EventPage -> IO (Result ([CalendarEvent], Bool))
    next pages scanned events ids seen response = case collectEvents rules zones first lastTime entry scanned events ids (fromMaybe [] (eventsItems response)) of
      Left issue -> pure (Left issue)
      Right (count, values, known, cut) -> advanceEvents pages count values known seen cut (eventsNextPageToken response)
    advanceEvents :: Int -> Int -> [CalendarEvent] -> Set.Set Text -> [Text] -> Bool -> Maybe Text -> IO (Result ([CalendarEvent], Bool))
    advanceEvents _ _ values _ _ True _ = pure (Right (values, True))
    advanceEvents pages count values known seen False token = case token of
      Nothing -> pure (Right (values, False))
      Just "" -> pure (Right (values, False))
      Just page | Text.length page > calendarTokenChars rules || page `elem` seen -> pure (Left (failure "calendar_invalid_pagination"))
      Just page -> loop (pages + 1) count values known (page : seen) (Just page)

collectEvents :: CalendarRules -> TimeZones -> UTCTime -> UTCTime -> CalendarEntry -> Int -> [CalendarEvent] -> Set.Set Text -> [Value] -> Result (Int, [CalendarEvent], Set.Set Text, Bool)
collectEvents _ _ _ _ _ scanned found ids [] = Right (scanned, found, ids, False)
collectEvents rules zones first lastTime entry scanned found ids (value : rest)
  | scanned >= calendarEventScanLimit rules = Right (scanned, found, ids, True)
  | otherwise = decodeValue "calendar_invalid_response" value >>= collectEvent rules zones first lastTime entry scanned found ids rest

collectEvent :: CalendarRules -> TimeZones -> UTCTime -> UTCTime -> CalendarEntry -> Int -> [CalendarEvent] -> Set.Set Text -> [Value] -> Event -> Result (Int, [CalendarEvent], Set.Set Text, Bool)
collectEvent rules zones first lastTime entry scanned found ids rest event
  | not (visible event) = next found ids
  | otherwise = bounds zones (entryTimeZone entry) event >>= include
  where
    ident = fromMaybe "" (eventId event)
    next :: [CalendarEvent] -> Set.Set Text -> Result (Int, [CalendarEvent], Set.Set Text, Bool)
    next values known = collectEvents rules zones first lastTime entry (scanned + 1) values known rest
    include :: (UTCTime, UTCTime, Bool) -> Result (Int, [CalendarEvent], Set.Set Text, Bool)
    include period@(begin, end, _)
      | end <= first || begin >= lastTime = next found ids
      | Text.null ident || Text.length ident > calendarIdChars rules = Left (failure "calendar_invalid_response")
      | Set.member ident ids = next found ids
      | otherwise = summarizeEvent rules zones entry event period >>= appendSummary begin
    appendSummary :: UTCTime -> EventSummary -> Result (Int, [CalendarEvent], Set.Set Text, Bool)
    appendSummary begin summaryValue = next (found <> [(begin, summaryValue)]) (Set.insert ident ids)

row :: CalendarRules -> CalendarEntry -> Text -> Maybe Text -> CalendarRow
row rules entry
  = CalendarRow
      (entryId entry) (entryName rules entry)
      (Nullable (entryTimeZone entry))
      (fromMaybe "" (entryAccessRole entry))
      (fromMaybe False (entryPrimary entry))

selectCalendars :: CalendarRules -> Text -> [CalendarEntry] -> ([CalendarEntry], Maybe Text)
selectCalendars rules selection entries
  | Text.null selection = (entries, Nothing)
  | otherwise = case chosen of
      [entry] -> ([entry], Nothing)
      [] -> (entries, Just "calendar_not_found")
      _ -> (entries, Just "calendar_ambiguous")
  where
    exact = filter (\entry -> entryId entry == selection || selection == "primary" && entryPrimary entry == Just True) entries
    labels = filter (\entry -> Text.toCaseFold (entryName rules entry) == Text.toCaseFold selection) entries
    chosen = case exact of
      [] -> labels
      _ -> exact

data Period = Period {periodStart :: UTCTime, periodEnd :: UTCTime}
data AgendaMode = SingleDay | DateRange
data AgendaRequest = AgendaRequest
  { agendaMode :: AgendaMode, agendaArguments :: SearchArgs, agendaPeriod :: Period }
data CreateRequest = CreateRequest {createArguments :: CreateArgs, createPeriod :: Period}
data CalendarCommand = ReadAgenda AgendaRequest | CreateEvent CreateRequest

type ScannedCalendar = ([CalendarEvent], CalendarRow)

scan :: CalendarRules -> Send -> TimeZones -> Period -> Text -> Text -> IO ([CalendarEvent], Coverage)
scan rules send zones period query selection = do
  discovery <- listCalendars rules send
  let (entries, selectionError) = selectCalendars rules selection (discoveredEntries discovery)
      tasks = scheduleScans rules selectionError entries
  results <- boundedMap (calendarConcurrency rules) scanOne tasks
  pure (scanResults discovery selectionError results)
  where
    scanOne :: (CalendarEntry, Maybe Text) -> IO ScannedCalendar
    scanOne (entry, Just reason) = pure ([], row rules entry "skipped" (Just reason))
    scanOne (entry, Nothing) = do
      result <- listEvents rules send zones (periodStart period) (periodEnd period) query entry
      pure (scannedCalendar rules entry result)

scheduleScans :: CalendarRules -> Maybe Text -> [CalendarEntry] -> [(CalendarEntry, Maybe Text)]
scheduleScans rules selectionError = snd . mapAccumL choose 0
  where
    choose :: Int -> CalendarEntry -> (Int, (CalendarEntry, Maybe Text))
    choose count entry = case skipReason count entry of
      Just reason -> (count, (entry, Just reason))
      Nothing -> (count + 1, (entry, Nothing))
    skipReason :: Int -> CalendarEntry -> Maybe Text
    skipReason count entry
      | isJust selectionError = selectionError
      | entryAccessRole entry == Just "freeBusyReader" = Just "free_busy_only"
      | fromMaybe "" (entryAccessRole entry) `notElem` calendarReadableRoles rules = Just "unreadable_access_role"
      | count >= calendarCalendarsChecked rules = Just "calendar_limit"
      | otherwise = Nothing

scannedCalendar :: CalendarRules -> CalendarEntry -> Result ([CalendarEvent], Bool) -> ScannedCalendar
scannedCalendar rules entry result = case result of
  Left issue -> ([], row rules entry "failed" (Just (failureCode issue)))
  Right (events, True) -> (events, row rules entry "partial" (Just "event_scan_limit"))
  Right (events, False) -> (events, row rules entry "complete" Nothing)

scanResults :: Discovery -> Maybe Text -> [ScannedCalendar] -> ([CalendarEvent], Coverage)
scanResults discovery selectionError results = (events, coverage)
  where
    rows = map snd results
    events = concatMap fst results
    complete = discoveredComplete discovery && isNothing selectionError && all ((== "complete") . rowStatus) rows
    checkedCount = length (filter (\item -> rowStatus item `elem` ["complete", "partial"]) rows)
    truncated = not (discoveredComplete discovery) && isNothing (discoveredError discovery)
    coverage = Coverage complete (discoveredComplete discovery) (Nullable (discoveredError discovery)) truncated (length rows) checkedCount rows selectionError

agenda :: CalendarRules -> Send -> TimeZones -> AgendaRequest -> IO Value
agenda rules send zones request = do
  result <- scan rules send zones (agendaPeriod request) (searchQuery args) (searchCalendar args)
  pure (agendaResult rules request result)
  where args = agendaArguments request

agendaResult :: CalendarRules -> AgendaRequest -> ([CalendarEvent], Coverage) -> Value
agendaResult rules request (events, coverage) = combine [toJSON output, toJSON changed]
  where
    args = agendaArguments request
    ordered = map snd (sortOn fst events)
    remaining = calendarOutputBytes rules - referenceSize coverage
    (bounded, cut) = limitJson (calendarOutputCount rules) remaining ordered
    truncated = cut || not (coverageCoverageComplete coverage)
    changed = coverage {coverageCoverageComplete = not truncated}
    problem = coverageError coverage
    status = maybe "ok" (const "error") problem
    (date, start, end, query, selection) = agendaFields (agendaMode request) args
    output = Agenda status date start end query selection (calendarZone rules) bounded (length bounded) truncated truncated cut True problem

agendaFields :: AgendaMode -> SearchArgs -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
agendaFields SingleDay args = (Just (searchStartDate args), Nothing, Nothing, Nothing, Nothing)
agendaFields DateRange args = (Nothing, Just (searchStartDate args), Just (searchEndDate args), Just (searchQuery args), Just (searchCalendar args))

coverageError :: Coverage -> Maybe Text
coverageError coverage
  | coverageCalendarsChecked coverage > 0 = Nothing
  | isJust (nullable (coverageCalendarListError coverage)) = Just "calendar_discovery_failed"
  | isJust (coverageCalendarSelectionError coverage) = coverageCalendarSelectionError coverage
  | any ((== "failed") . rowStatus) (coverageCalendarResults coverage) = Just "calendar_events_unavailable"
  | otherwise = Nothing

createEvent :: CalendarRules -> Send -> Durable -> Existing -> TimeZones -> Text -> CreateRequest -> IO (Result Value)
createEvent rules send durable existing zones requestId request = do
  previous <- existing requestId "calendar_create" normalized
  either (pure . Left) (maybe create (pure . Right)) previous
  where
    args = createArguments request
    normalized = toJSON args
    resource = creationResource rules requestId args
    commit = durable requestId "calendar_create" normalized operation
    create :: IO (Result Value)
    create
      | createAllowConflicts args = commit
      | otherwise = checkConflicts
    checkConflicts :: IO (Result Value)
    checkConflicts = do
      result <- scan rules send zones (createPeriod request) "" ""
      maybe commit (pure . Right) (conflictResult rules result)
    operation = do
      response <- post (calendarHttp rules) send (eventUrl rules "primary") params resource
      pure (response >>= creationReceipt rules zones (createPeriod request) resource)
    updates = case resourceAttendees resource of
      [] -> "none"
      _ -> "all"
    params = [textParam "sendUpdates" updates]

conflictResult :: CalendarRules -> ([CalendarEvent], Coverage) -> Maybe Value
conflictResult rules (events, coverage)
  | not (null busy) = Just (combine [toJSON conflict, toJSON coverage])
  | not (coverageCoverageComplete coverage) = Just (combine [toJSON incomplete, toJSON coverage])
  | otherwise = Nothing
  where
    incomplete = Incomplete "error" "calendar_conflict_check_incomplete" False True
    busy = map snd (sortOn fst (filter (sumBusy . snd) events))
    conflict = Conflict "error" "schedule_conflict" False (take (calendarConflictCount rules) busy) (length busy > calendarConflictCount rules || not (coverageCoverageComplete coverage)) True

creationResource :: CalendarRules -> Text -> CreateArgs -> EventResource
creationResource rules requestId args = EventResource ident (createSummary args) start end (createDescription args) (createLocation args) recipients
  where
    rawId = Text.encodeUtf8 ("phone-assistant-calendar:" <> requestId)
    ident = Text.pack (show (hash rawId :: Digest SHA256))
    recipients = map Email (createAttendees args)
    start = EventTime Nothing (Just (createStart args)) (Just (calendarZone rules))
    end = EventTime Nothing (Just (createEnd args)) (Just (calendarZone rules))

creationReceipt :: CalendarRules -> TimeZones -> Period -> EventResource -> Value -> Result Value
creationReceipt rules zones expected resource value = Bifunctor.first uncertain decoded
  where
    decoded = decodeValue "calendar_create_response_missing_receipt" value >>= receipt
    uncertain :: Failure -> Failure
    uncertain issue = issue {failureUncertain = True}
    receipt :: Event -> Result Value
    receipt response
      | eventId response /= Just (resourceId resource) || eventStatus response `notElem` [Just "confirmed", Just "tentative"] = Left (failure "calendar_create_response_missing_receipt")
      | otherwise = confirmReceipt rules zones expected resource response

confirmReceipt :: CalendarRules -> TimeZones -> Period -> EventResource -> Event -> Result Value
confirmReceipt rules zones expected resource response = case bounds zones (Just (calendarZone rules)) response of
  Right actual@(first, lastTime, False)
    | first == periodStart expected && lastTime == periodEnd expected -> render actual
  _ -> Left (failure "calendar_create_response_invalid_receipt")
  where
    entry = CalendarEntry "primary" (Just (calendarPrimaryName rules)) Nothing (Just "owner") (Just True) Nothing (Just (calendarZone rules))
    recipients = map emailEmail (resourceAttendees resource)
    render :: (UTCTime, UTCTime, Bool) -> Result Value
    render actual = do
      summaryValue <- summarizeEvent rules zones entry response actual
      let created = Created "ok" True summaryValue recipients (not (null recipients)) False
      Right (toJSON created)

prepareCreate :: CalendarRules -> TimeZones -> CreateArgs -> Result CreateRequest
prepareCreate rules zones args
  | length (createAttendees args) > calendarAttendeeLimit rules = Left (failure "invalid_calendar_attendees")
  | otherwise = do
      title <- textValue (calendarCreateTitleChars rules) True (createSummary args)
      description <- optionalText (calendarDescriptionChars rules) False (createDescription args)
      location <- optionalText (calendarCreateLocationChars rules) True (createLocation args)
      period <- parsePeriod (createStart args) (createEnd args)
      recipients <- traverse address (createAttendees args)
      start <- zonedInstant zones (calendarZone rules) (periodStart period)
      end <- zonedInstant zones (calendarZone rules) (periodEnd period)
      let normalized = CreateArgs title (isoZoned start) (isoZoned end) description location (nub recipients) (createAllowConflicts args)
      Right (CreateRequest normalized period)

parsePeriod :: Text -> Text -> Result Period
parsePeriod start end = do
  first <- parseInstant start
  lastTime <- parseInstant end
  case Period first lastTime of
    period | lastTime > first -> Right period
    _ -> Left (failure "calendar_end_must_follow_start")

optionalText :: Int -> Bool -> Text -> Result Text
optionalText _ _ "" = Right ""
optionalText cap header value = textValue cap header value

prepareSearch :: CalendarRules -> TimeZones -> SearchArgs -> Result AgendaRequest
prepareSearch rules zones args = do
  first <- parseDay (searchStartDate args)
  lastDay <- parseDay (searchEndDate args)
  prepareDateRange rules zones args first lastDay

prepareDateRange :: CalendarRules -> TimeZones -> SearchArgs -> Day -> Day -> Result AgendaRequest
prepareDateRange rules zones args first lastDay
  | lastDay < first || lastDay >= fromGregorian 9999 12 31 = Left (failure "invalid_calendar_date_range")
  | otherwise = do
      query <- optionalText (calendarFilterChars rules) True (searchQuery args)
      selection <- optionalText (calendarFilterChars rules) True (searchCalendar args)
      period <- datePeriod rules zones first lastDay
      let normalized = args {searchQuery = query, searchCalendar = selection}
      Right (AgendaRequest DateRange normalized period)

prepareAgenda :: CalendarRules -> TimeZones -> AgendaArgs -> Result AgendaRequest
prepareAgenda rules zones args = do
  day <- parseDay (agendaDate args)
  period <- datePeriod rules zones day day
  let normalized = SearchArgs (agendaDate args) (agendaDate args) "" ""
  Right (AgendaRequest SingleDay normalized period)

datePeriod :: CalendarRules -> TimeZones -> Day -> Day -> Result Period
datePeriod rules zones firstDay lastDay = do
  let following = addDays 1 lastDay
      beginning = LocalTime firstDay midnight
      end = LocalTime following midnight
  first <- localInstant zones (calendarZone rules) beginning
  lastTime <- localInstant zones (calendarZone rules) end
  Right (Period first lastTime)

prepareCommand :: CalendarRules -> TimeZones -> Text -> Value -> Result CalendarCommand
prepareCommand rules zones name arguments = case name of
  "calendar_agenda" -> ReadAgenda <$> (decoded >>= prepareAgenda rules zones)
  "calendar_search" -> ReadAgenda <$> (decoded >>= prepareSearch rules zones)
  "calendar_create" -> CreateEvent <$> (decoded >>= prepareCreate rules zones)
  _ -> Left (failure "unsupported_calendar_tool")
  where
    decoded :: FromJSON a => Result a
    decoded = decodeValue "invalid_tool_arguments" arguments

calendar :: CalendarRules -> Send -> Durable -> Existing -> TimeZones -> Execute
calendar rules send durable existing zones name arguments requestId = either (pure . Left) execute prepared
  where
    prepared = prepareCommand rules zones name arguments
    execute :: CalendarCommand -> IO (Result Value)
    execute (ReadAgenda request) = Right <$> agenda rules send zones request
    execute (CreateEvent request) = createEvent rules send durable existing zones requestId request

data Incomplete = Incomplete
  { incompleteStatus :: Text, incompleteError :: Text, incompleteRetrySafe :: Bool
  , incompleteUntrustedCalendarContent :: Bool } deriving (Generic)
instance ToJSON Incomplete where toJSON = genericToJSON (jsonOptions 10)
