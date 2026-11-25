module NotesSpec (spec) where

import Baerbel.Error (Result, failure)
import Baerbel.JSON (decodeBytes, jsonOptions)
import Baerbel.Notes
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson hiding (Result)
import qualified Data.ByteString as Bytes
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Test.Hspec
import TestSupport (withRule)

data Observed = Observed
  { observedSequence :: Int, observedText :: Text, observedStart :: Maybe Double, observedEnd :: Maybe Double } deriving (Generic)
instance FromJSON Observed where parseJSON = genericParseJSON (jsonOptions 8)
data Snapshot = Snapshot
  { snapshotFragments :: [Observed], snapshotLastDelta :: Double, snapshotVoiceStarted :: Double
  , snapshotNow :: Double, snapshotArchiveBound :: Bool, snapshotSequence :: Int, snapshotDroppedThrough :: Int } deriving (Generic)
instance FromJSON Snapshot where parseJSON = genericParseJSON (jsonOptions 8)
data Step = Step
  { stepTool :: Text, stepArguments :: Value, stepSnapshot :: Snapshot, stepLater :: Maybe Snapshot
  , stepSettled :: Bool, stepExpected :: Value, stepPending :: Bool, stepDeliveries :: [(Text, Text)], stepLogs :: [Value] } deriving (Generic)
instance FromJSON Step where parseJSON = genericParseJSON (jsonOptions 4)
data Workflow = Workflow { workflowLabel :: Text, workflowAudio :: Text, workflowReceipt :: Value, workflowSteps :: [Step] } deriving (Generic)
instance FromJSON Workflow where parseJSON = genericParseJSON (jsonOptions 8)

transcript :: Snapshot -> Transcript
transcript value = Transcript (map fragment (snapshotFragments value)) (snapshotLastDelta value)
  (snapshotVoiceStarted value) (snapshotNow value) (snapshotArchiveBound value)
  (snapshotSequence value) (snapshotDroppedThrough value)
  where fragment part = Fragment (observedSequence part) (observedText part) (observedStart part) (observedEnd part)

emptyTranscript :: Transcript
emptyTranscript = Transcript [] 0 0 10 True 0 0

nextSnapshot :: IORef [Transcript] -> IO Transcript
nextSnapshot ref = atomicModifyIORef' ref update
  where
    update [] = ([], emptyTranscript)
    update [value] = ([value], value)
    update (value : rest) = (rest, value)

appendRef :: IORef [a] -> a -> IO ()
appendRef ref value = atomicModifyIORef' ref (\values -> (values <> [value], ()))

workflow :: NoteRules -> Workflow -> Expectation
workflow rules fixture = do
  snapshots <- newIORef [emptyTranscript]
  deliveries <- newIORef []
  identifiers <- newIORef (0 :: Int)
  logs <- newIORef []
  let identifier = atomicModifyIORef' identifiers (\count -> let next = count + 1 in (next, "note-" <> Text.pack (show next)))
      deliver ident speech _ = appendRef deliveries (ident, speech) >> pure (Right (workflowReceipt fixture))
      report action state code = appendRef logs (object ["action" .= action, "state" .= state, "error" .= code])
      effects = NoteEffects (nextSnapshot snapshots) (\_ _ -> pure (audio rules (workflowAudio fixture))) deliver (pure ()) identifier report
  (execute, busy, close) <- notes rules effects
  mapM_ (runStep snapshots deliveries logs execute busy close) (workflowSteps fixture)
  close

audio :: NoteRules -> Text -> Result Bytes.ByteString
audio _ "error" = Left (failure "fixture_archive_error")
audio _ "empty" = Right Bytes.empty
audio rules "large" = let size = noteMaximumBytes rules + 1 in Right (Bytes.replicate size 0)
audio _ _ = Right "pcm"

runStep :: IORef [Transcript] -> IORef [(Text, Text)] -> IORef [Value]
  -> (Text -> Value -> Text -> IO (Result Value)) -> IO Bool -> IO () -> Step -> Expectation
runStep snapshots deliveries logs execute busy close step = do
  let initial = transcript (stepSnapshot step)
      later = maybe initial transcript (stepLater step)
      final | stepSettled step = later
            | otherwise = later {transcriptNow = transcriptLastDelta later}
  writeIORef snapshots [initial, final]
  writeIORef logs []
  let run | stepTool step == "__close" = close
          | otherwise = execute (stepTool step) (stepArguments step) "same-request" `shouldReturn` Right (stepExpected step)
  run
  busy `shouldReturn` stepPending step
  readIORef deliveries `shouldReturn` stepDeliveries step
  readIORef logs `shouldReturn` stepLogs step

cancelledReceipt :: NoteRules -> Expectation
cancelledReceipt rules = do
  snapshot <- newIORef emptyTranscript
  accepted <- newEmptyMVar
  released <- newEmptyMVar
  delivered <- newEmptyMVar
  let receipt = object ["status" .= ("ok" :: Text), "result" .= ("sent" :: Text)]
      deliver _ _ _ = putMVar accepted () >> takeMVar released >> putMVar delivered () >> pure (Right receipt)
      effects = NoteEffects (readIORef snapshot) (\_ _ -> pure (Right "pcm")) deliver (pure ()) (pure "note-id") (\_ _ _ -> pure ())
      arguments = object ["completion_phrase" .= ("send it" :: Text)]
      unknown = object ["status" .= ("unknown" :: Text), "error" .= ("note_send_outcome_unknown_do_not_retry" :: Text)]
  (execute, busy, close) <- notes rules effects
  _ <- execute "owner_note_start" (object ["start_quote" .= ("" :: Text)]) "request"
  writeIORef snapshot (Transcript [Fragment 1 "send it" (Just 1) (Just 2)] 0 0 10 True 1 0)
  caller <- async (execute "owner_note_send" arguments "send")
  takeMVar accepted
  cancel caller
  execute "owner_note_send" arguments "repeat" `shouldReturn` Right unknown
  busy `shouldReturn` True
  putMVar released ()
  takeMVar delivered
  execute "owner_note_send" arguments "repeat-after-delivery" `shouldReturn` Right unknown
  close

spec :: Spec
spec = describe "owner note workflow equivalence" $ do
  bytes <- runIO (Bytes.readFile "test/fixtures/owner-notes-workflow.json")
  case decodeBytes "owner_note_fixtures" bytes :: Result [Workflow] of
    Left issue -> it "loads original note fixtures" (expectationFailure (show issue))
    Right cases -> mapM_ fixtureExample cases
  it "preserves the unknown receipt when the tool caller cancels an accepted delivery" $
    withRule "notes" "selection" cancelledReceipt
  where fixtureExample fixture = it (Text.unpack (workflowLabel fixture)) (withRule "notes" "selection" (`workflow` fixture))
