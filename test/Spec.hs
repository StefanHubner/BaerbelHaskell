module Main (main) where

import qualified EffectsSpec
import qualified NotesSpec
import qualified MailContentSpec
import qualified MetricsSpec
import qualified MessagesSpec
import qualified NoteMailSpec
import qualified EventsSpec
import qualified HTTPSpec
import qualified LoggingSpec
import qualified AuthSpec
import qualified ConfigPathsSpec
import qualified ContactsSpec
import qualified ToolsSpec
import qualified BoundarySpec
import qualified StoreSpec
import qualified CoreSpec
import qualified LiveSpec
import qualified TelephonySpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  EffectsSpec.spec
  NotesSpec.spec
  MetricsSpec.spec
  MessagesSpec.spec
  MailContentSpec.spec
  NoteMailSpec.spec
  EventsSpec.spec
  HTTPSpec.spec
  LoggingSpec.spec
  AuthSpec.spec
  CoreSpec.spec
  StoreSpec.spec
  BoundarySpec.spec
  ConfigPathsSpec.spec
  ContactsSpec.spec
  ToolsSpec.spec
  LiveSpec.spec
  TelephonySpec.spec
