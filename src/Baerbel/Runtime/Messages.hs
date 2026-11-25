module Baerbel.Runtime.Messages
  ( MessageRules(..), Context, Messages(..), withMessages, emptyContext, outcomeText ) where

import Baerbel.Core.Error ( Result )
import Baerbel.Core.Incoming ( Decision(..) )
import Baerbel.Core.Messages
import Baerbel.Runtime.Logging ( Report )
import Baerbel.Runtime.SMS
    ( SMS, Delivery(..), Kind(..), Receipt(..) )
import Control.Concurrent ( forkFinally )
import Control.Concurrent.STM
    ( atomically,
      check,
      STM,
      TVar,
      newTVar,
      newTVarIO,
      readTVar,
      writeTVar,
      newEmptyTMVar,
      putTMVar,
      readTMVar,
      modifyTVar',
      TMVar )
import Control.Exception ( finally, mask )
import Control.Monad ( void, when )
import Data.Aeson ( KeyValue((.=)) )
import Data.Either ( fromRight )
import Data.List ( sortOn )
import Data.Map.Strict ( Map )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import System.Timeout ( timeout )
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

data Context = Context {contextOrder :: Integer, contextValue :: TVar ScreeningContext}
data Messages = Messages
  { beginContext :: Text -> IO (Maybe Context)
  , finishContext :: Context -> IO ()
  , observeContext :: Context -> Text -> IO ()
  , decideContext :: Context -> Decision -> IO ()
  , knownContext :: Context -> Text -> IO ()
  , deliverMessage :: Text -> Text -> Text -> Text -> Maybe Text -> IO Receipt
  , deliverOutcome :: Text -> Text -> Maybe Text -> Bool -> IO Receipt
  }
data TextSource = ConstantText Text | ContextText (ScreeningContext -> Text)

data Job = Job {jobKind :: Kind, jobResult :: TMVar Receipt, jobStarted :: TVar Bool}
data JobPlan = RejectJob | UseJob Job | ReplaceJob (Maybe Job)
data State = State
  { stateJobs :: Map Text Job, stateContexts :: Map Text Context
  , statePending :: Int, stateClosed :: Bool, stateNextContext :: Integer }

withMessages :: MessageRules -> Report -> SMS -> (Messages -> IO (Result a)) -> IO (Result a)
withMessages rules report sender action = do
  state <- newTVarIO (State Map.empty Map.empty 0 False 0)
  let observe context delta = updateContext context (appendExcerpt rules delta)
      decide context decision = updateContext context (decided rules decision)
      name context value = updateContext context (known value)
      service = Messages (begin rules state) (`updateContext` finish) observe decide name
        (submitMessage rules report sender state) (submitOutcome rules report sender state)
  finally (action service) (drain state)

begin :: MessageRules -> TVar State -> Text -> IO (Maybe Context)
begin rules state ident = atomically (readTVar state >>= allocate rules state ident)

allocate :: MessageRules -> TVar State -> Text -> State -> STM (Maybe Context)
allocate rules state ident current
  | stateClosed current = pure Nothing
  | otherwise = case Map.lookup ident (stateContexts current) of
      Just context -> activate context
      Nothing -> createContext rules state ident current

activate :: Context -> STM (Maybe Context)
activate context = modifyTVar' (contextValue context) increment >> pure (Just context)
  where increment current = current {contextActive = contextActive current + 1}

createContext :: MessageRules -> TVar State -> Text -> State -> STM (Maybe Context)
createContext rules state ident current = do
  available <- traverse inactive (sortOn (contextOrder . snd) (Map.toList (stateContexts current)))
  let contexts = chooseContexts (messageContextLimit rules) (stateContexts current) available
  maybe (pure Nothing) insert contexts
  where
  inactive (key, Context _ value) = fmap (\context -> (key, contextActive context == 0)) (readTVar value)
  insert contexts = do
    value <- newTVar emptyContext {contextActive = 1}
    let context = Context (stateNextContext current) value
    writeTVar state current {stateContexts = Map.insert ident context contexts, stateNextContext = stateNextContext current + 1}
    pure (Just context)

updateContext :: Context -> (ScreeningContext -> ScreeningContext) -> IO ()
updateContext (Context _ value) change = atomically (modifyTVar' value change)

submitMessage :: MessageRules -> Report -> SMS -> TVar State -> Text -> Text -> Text -> Text -> Maybe Text -> IO Receipt
submitMessage rules report sender state ident call name body callback = submit rules (reported rules report ident (Just call) sender) state ident Message (ConstantText text)
  where
  named = cleanName rules name
  caller | Text.null named = render rules "caller" []
         | otherwise = named
  number = maybe "" (\value -> " (" <> value <> ")") callback
  text = render rules "message" [("name", caller <> number), ("message", Text.strip body)]

submitOutcome :: MessageRules -> Report -> SMS -> TVar State -> Text -> Text -> Maybe Text -> Bool -> IO Receipt
submitOutcome rules report sender state ident reason callback answered
  | answered || reason == "ANSWERED" = atomically (modifyTVar' state forget) >> pure (Receipt Skipped Nothing Nothing)
  | otherwise = submit rules (reported rules report ident Nothing sender) state ident Outcome text
  where
  forget current = current {stateContexts = Map.delete ident (stateContexts current)}
  text = ContextText (outcomeText rules reason callback)

snapshot :: TVar State -> Text -> IO ScreeningContext
snapshot state ident = atomically $ do
  current <- readTVar state
  maybe (pure emptyContext) (\(Context _ value) -> readTVar value) (Map.lookup ident (stateContexts current))

submit :: MessageRules -> SMS -> TVar State -> Text -> Kind -> TextSource -> IO Receipt
submit rules sender state ident kind text = mask $ \restore -> do
  selected <- atomically (selectJob rules state ident kind)
  case selected of
    Nothing -> pure (unsubmitted kind)
    Just (job, Nothing) -> restore (atomically (readTMVar (jobResult job)))
    Just (job, Just previous) -> do
      void (forkFinally (restore (deliverJob rules sender state ident job previous text)) (completeJob state ident job))
      restore (atomically (readTMVar (jobResult job)))

selectJob :: MessageRules -> TVar State -> Text -> Kind -> STM (Maybe (Job, Maybe (Maybe Job)))
selectJob rules state ident kind = do
  current <- readTVar state
  let previous = Map.lookup ident (stateJobs current)
  started <- maybe (pure False) (readTVar . jobStarted) previous
  case planJob rules current kind started previous of
    RejectJob -> pure Nothing
    UseJob job -> pure (Just (job, Nothing))
    ReplaceJob earlier -> newJob state ident kind current earlier

planJob :: MessageRules -> State -> Kind -> Bool -> Maybe Job -> JobPlan
planJob rules current kind started previous
  | stateClosed current || outcomeStarted = RejectJob
  | not replace = maybe RejectJob UseJob previous
  | statePending current >= messageTaskLimit rules = RejectJob
  | otherwise = ReplaceJob previous
  where
    replace = case previous of
      Nothing -> True
      Just job -> kind == Outcome && jobKind job == Message || kind == Message && jobKind job == Outcome && not started
    outcomeStarted = kind == Message && started && maybe False ((== Outcome) . jobKind) previous

newJob :: TVar State -> Text -> Kind -> State -> Maybe Job -> STM (Maybe (Job, Maybe (Maybe Job)))
newJob state ident kind current previous = do
  result <- newEmptyTMVar
  started <- newTVar False
  let job = Job kind result started
  writeTVar state current {stateJobs = Map.insert ident job (stateJobs current), statePending = statePending current + 1}
  pure (Just (job, Just previous))

deliverJob :: MessageRules -> SMS -> TVar State -> Text -> Job -> Maybe Job -> TextSource -> IO Receipt
deliverJob rules sender state ident job previous text = do
  inherited <- previousResult
  case receiptStatus inherited of
    Failed -> sendNew
    _ -> pure inherited
  where
  previousResult = case previous of
    Just earlier | jobKind job == Outcome -> atomically (readTMVar (jobResult earlier))
    _ -> pure (Receipt Failed Nothing Nothing)
  sendNew = do
    when (jobKind job == Outcome) (waitContext rules state ident)
    replacement <- atomically (replacementJob state ident job)
    inherited <- maybe (pure (Receipt Failed Nothing Nothing)) (atomically . readTMVar . jobResult) replacement
    case receiptStatus inherited of
      Failed -> perform
      _ -> pure inherited
  perform = do
    when (jobKind job == Outcome) (atomically (writeTVar (jobStarted job) True))
    sendText text
  sendText (ConstantText body) = sender ("incoming:" <> ident) body (jobKind job)
  sendText (ContextText renderBody) = do
    context <- snapshot state ident
    sender ("incoming:" <> ident) (renderBody context) (jobKind job)

waitContext :: MessageRules -> TVar State -> Text -> IO ()
waitContext rules state ident = void (timeout (messageWaitMicros rules) (atomically ready))
  where
  ready = do
    current <- readTVar state
    case Map.lookup ident (stateContexts current) of
      Nothing -> pure ()
      Just (Context _ value) -> readTVar value >>= check . (== 0) . contextActive

replacementJob :: TVar State -> Text -> Job -> STM (Maybe Job)
replacementJob state ident job = do
  current <- readTVar state
  case Map.lookup ident (stateJobs current) of
    Just replacement | jobKind job == Outcome && jobKind replacement == Message -> pure (Just replacement)
    _ -> writeTVar (jobStarted job) True >> pure Nothing

completeJob :: TVar State -> Text -> Job -> Either e Receipt -> IO ()
completeJob state ident job outcome = atomically $ do
  let receipt = fromRight unknownReceipt outcome
  putTMVar (jobResult job) receipt
  modifyTVar' state (completed ident job receipt)

completed :: Text -> Job -> Receipt -> State -> State
completed ident job receipt state = state {statePending = statePending state - 1, stateJobs = jobs, stateContexts = contexts}
  where
  current = Map.lookup ident (stateJobs state)
  matches = maybe False (\value -> jobResult value == jobResult job) current
  jobs | matches = Map.delete ident (stateJobs state)
       | otherwise = stateJobs state
  contexts | jobKind job == Outcome && receiptRetryable receipt /= Just True = Map.delete ident (stateContexts state)
           | otherwise = stateContexts state

drain :: TVar State -> IO ()
drain state = atomically close >> atomically await
  where
  close = do
    current <- readTVar state
    mapM_ finishValue (Map.elems (stateContexts current))
    writeTVar state current {stateClosed = True}
  finishValue (Context _ value) = modifyTVar' value (\current -> current {contextActive = 0})
  await = readTVar state >>= check . (== 0) . statePending

reported :: MessageRules -> Report -> Text -> Maybe Text -> SMS -> SMS
reported rules report ident call sender key body kind = do
  result <- timeout (messageDeliveryMicros rules) (sender key body kind)
  let receipt = normalizeReceipt (fromMaybe unknownReceipt result)
      status = case receiptStatus receipt of
        Sent -> "sent"
        Failed -> "failed"
        _ -> "uncertain" :: Text
  report "incoming_message_delivery" ["call_id" .= call, "event_id" .= ident, "status" .= status]
  pure receipt
