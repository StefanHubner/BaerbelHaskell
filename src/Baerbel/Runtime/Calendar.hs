{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Calendar (module Baerbel.Core.Calendar, calendar) where

import Baerbel.Core.Calendar
import Baerbel.Core.Error ( Result )
import Baerbel.Core.JSON ( decodeValue )
import Baerbel.Core.ProviderConfig ( CalendarRules(..) )
import Baerbel.Runtime.Effects ( continue )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Baerbel.Runtime.Provider
    ( Existing, Durable, Execute, boundedMap )
import Data.Aeson hiding (Result)
import Data.Text ( Text )
import Data.Time ( UTCTime )

listCalendars :: CalendarRules -> Send -> IO Discovery
listCalendars rules send = loop initialPage
  where
  loop :: PageState CalendarEntry -> IO Discovery
  loop state
    | pageNumber state >= calendarListPages rules || pageScanned state >= calendarCalendarLimit rules = pure (Discovery (pageItems state) False Nothing)
    | otherwise = do
      let request = calendarPageRequest rules state
      response <- requestJSON send request (decodeValue "calendar_invalid_calendar_list")
      either pure loop (calendarPage rules state response)

listEvents :: CalendarRules -> Send -> TimeZones -> UTCTime -> UTCTime -> Text -> CalendarEntry -> IO (Result ([CalendarEvent], Bool))
listEvents rules send zones first lastTime query entry = loop initialPage
  where
  loop :: PageState CalendarEvent -> IO (Result ([CalendarEvent], Bool))
  loop state
    | pageNumber state >= calendarEventPages rules || pageScanned state >= calendarEventScanLimit rules = pure (Right (pageItems state, True))
    | otherwise = do
      let request = eventPageRequest rules first lastTime query entry state
      response <- requestJSON send request (decodeValue "calendar_invalid_response")
      either pure loop (eventPage rules zones first lastTime entry state response)

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

createEvent :: CalendarRules -> Send -> Durable -> Existing -> TimeZones -> Text -> CreateRequest -> IO (Result Value)
createEvent rules send durable existing zones requestId request = do
  previous <- existing requestId "calendar_create" normalized
  continue (maybe create (pure . Right)) previous
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
      let wire = creationRequest rules resource
      requestJSON send wire (creationReceipt rules zones (createPeriod request) resource)

calendar :: CalendarRules -> Send -> Durable -> Existing -> TimeZones -> Execute
calendar rules send durable existing zones name arguments requestId = continue execute prepared
  where
    prepared = prepareCommand rules zones name arguments
    execute :: CalendarCommand -> IO (Result Value)
    execute (ReadAgenda request) = do
      let args = agendaArguments request
      result <- scan rules send zones (agendaPeriod request) (searchQuery args) (searchCalendar args)
      pure (Right (agenda rules request result))
    execute (CreateEvent request) = createEvent rules send durable existing zones requestId request
