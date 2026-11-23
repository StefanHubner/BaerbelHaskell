{-# LANGUAGE TupleSections #-}
module Baerbel.Messages
  ( MessageRules(..), Context, Messages(..), withMessages, emptyContext, outcomeText ) where

import Data.Either (fromRight)
import Baerbel.Error (Result)
import Baerbel.Incoming (Decision(..))
import Baerbel.Logging (Report)
import Baerbel.JSON (jsonOptions)
import Baerbel.SMS (SMS, Delivery(..), Kind(..), Receipt(..))
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
import Control.Exception (finally, mask)
import Control.Monad (void, when)
import Data.Aeson (FromJSON(..), genericParseJSON, (.=))
import Data.Char (ord)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Timeout (timeout)

data MessageRules = MessageRules
  { messageReasons :: Map Text Text, messageDeclined :: Map Text Text
  , messageForwarded :: Map Text Text, messageUnknownNames :: [Text]
  , messageTemplates :: Map Text Text, messageExcerptChars :: Int
  , messageNameChars :: Int, messageSummaryChars :: Int
  , messageContextLimit :: Int, messageTaskLimit :: Int
  , messageWaitMicros :: Int, messageDeliveryMicros :: Int } deriving (Generic)
instance FromJSON MessageRules where parseJSON = genericParseJSON (jsonOptions 7)
data ScreeningContext = ScreeningContext
  { contextName :: Text, contextSummary :: Text, contextDecision :: Text
  , contextReason :: Text, contextKnown :: Bool, contextExcerpt :: Text
  , contextTruncated :: Bool, contextActive :: Int } deriving (Eq, Show)
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
data State = State
  { stateJobs :: Map Text Job, stateContexts :: Map Text Context
  , statePending :: Int, stateClosed :: Bool, stateNextContext :: Integer }

emptyContext :: ScreeningContext
emptyContext = ScreeningContext "" "" "" "" False "" False 0

unknownReceipt :: Receipt
unknownReceipt = Receipt Uncertain Nothing Nothing

render :: MessageRules -> Text -> [(Text, Text)] -> Text
render rules key = foldl replace template
  where
  template = Map.findWithDefault "" key (messageTemplates rules)
  replace text (name, value) = Text.replace ("{" <> name <> "}") value text

cleanName :: MessageRules -> Text -> Text
cleanName rules value = if Text.toCaseFold name `elem` messageUnknownNames rules then "" else name
  where name = Text.strip value

sentence :: Text -> Text
sentence value
  | any (`Text.isSuffixOf` text) [".", "!", "?", "…"] = text
  | otherwise = text <> "."
  where text = Text.unwords (Text.words value)

outcomeText :: MessageRules -> Text -> Maybe Text -> ScreeningContext -> Text
outcomeText rules reason callback context = Text.intercalate " " parts
  where
  name = cleanName rules (contextName context)
  number = fromMaybe "" callback
  named = name <> if Text.null number then "" else " (" <> number <> ")"
  identityKey = if Text.null name then (if Text.null number then "anonymous" else "number") else "named"
  identity = render rules identityKey [("name", named), ("number", number)]
  base = Map.findWithDefault "" reason (messageReasons rules)
  description = describeOutcome rules reason context base
  excerpt = Text.unwords (Text.words (contextExcerpt context))
  quote = (if contextTruncated context then "…" else "") <> excerpt
  detail = if Text.null (contextSummary context) then render rules "excerpt" [("text", sentence quote)] else sentence (contextSummary context)
  hasDetail = not (Text.null excerpt && Text.null (contextSummary context))
  parts = [identity] ++ [detail | hasDetail] ++ [description]

describeOutcome :: MessageRules -> Text -> ScreeningContext -> Text -> Text
describeOutcome rules reason context fallback
  | reason == "SCREENING" && contextKnown context = render rules "known_hangup" []
  | reason `elem` ["DECLINED", "SCREENING_DECLINED"] = Map.findWithDefault fallback (contextReason context) (messageDeclined rules)
  | contextDecision context == "forward" = Map.findWithDefault fallback reason (messageForwarded rules)
  | otherwise = fallback

withMessages :: MessageRules -> Report -> SMS -> (Messages -> IO (Result a)) -> IO (Result a)
withMessages rules report sender action = do
  state <- newTVarIO (State Map.empty Map.empty 0 False 0)
  let service = Messages (begin rules state) finish (observe rules) (decide rules) known
        (submitMessage rules report sender state) (submitOutcome rules report sender state)
  finally (action service) (drain state)

begin :: MessageRules -> TVar State -> Text -> IO (Maybe Context)
begin rules state ident = atomically $ do
  current <- readTVar state
  if stateClosed current then pure Nothing else allocate rules state ident current

allocate :: MessageRules -> TVar State -> Text -> State -> STM (Maybe Context)
allocate rules state ident current = case Map.lookup ident contexts of
  Just context -> activate context
  Nothing -> createContext rules state ident current
  where contexts = stateContexts current

activate :: Context -> STM (Maybe Context)
activate context = modifyTVar' (contextValue context) increment >> pure (Just context)
  where increment current = current {contextActive = contextActive current + 1}

createContext :: MessageRules -> TVar State -> Text -> State -> STM (Maybe Context)
createContext rules state ident current = do
  available <- traverse inactive (sortOn (contextOrder . snd) (Map.toList (stateContexts current)))
  let removable = [key | (key, True) <- available]
      contexts = if Map.size (stateContexts current) < messageContextLimit rules then Just (stateContexts current)
        else case removable of key : _ -> Just (Map.delete key (stateContexts current)); [] -> Nothing
  maybe (pure Nothing) insert contexts
  where
  inactive (key, Context _ value) = fmap (\context -> (key, contextActive context == 0)) (readTVar value)
  insert contexts = do
    value <- newTVar emptyContext {contextActive = 1}
    let context = Context (stateNextContext current) value
    writeTVar state current {stateContexts = Map.insert ident context contexts, stateNextContext = stateNextContext current + 1}
    pure (Just context)

finish :: Context -> IO ()
finish (Context _ value) = atomically (modifyTVar' value decrement)
  where decrement current = current {contextActive = max 0 (contextActive current - 1)}

observe :: MessageRules -> Context -> Text -> IO ()
observe rules (Context _ value) delta = atomically (modifyTVar' value append)
  where
  clean = Text.filter allowed delta
  allowed char = (ord char >= 32 || char == '\n' || char == '\t') && not (ord char >= 127 && ord char <= 159) && not (ord char >= 0xd800 && ord char <= 0xdfff)
  append current = current {contextExcerpt = Text.takeEnd bound text, contextTruncated = contextTruncated current || Text.length text > bound}
    where text = contextExcerpt current <> clean; bound = messageExcerptChars rules

decide :: MessageRules -> Context -> Decision -> IO ()
decide rules (Context _ value) decision = atomically (modifyTVar' value (decided rules decision))

decided :: MessageRules -> Decision -> ScreeningContext -> ScreeningContext
decided _ (Forward reason name summary) = screenFacts "forward" reason name summary
decided _ (Decline reason name summary) = screenFacts "decline" reason name summary
decided rules (LeaveMessage name body) = \current -> current
  { contextDecision = "message", contextReason = "message", contextKnown = False
  , contextName = Text.take (messageNameChars rules) name
  , contextSummary = Text.take (messageSummaryChars rules) (Text.unwords (Text.words body)) }

screenFacts :: Text -> Text -> Text -> Text -> ScreeningContext -> ScreeningContext
screenFacts decision reason name summary current = current
  { contextDecision = decision, contextReason = reason
  , contextName = if Text.null name then contextName current else name
  , contextSummary = if Text.null summary then contextSummary current else summary
  , contextKnown = Text.null name && contextKnown current }

known :: Context -> Text -> IO ()
known (Context _ value) name = atomically (modifyTVar' value update)
  where update current = current {contextKnown = True, contextName = name, contextDecision = "forward"}

submitMessage :: MessageRules -> Report -> SMS -> TVar State -> Text -> Text -> Text -> Text -> Maybe Text -> IO Receipt
submitMessage rules report sender state ident call name body callback = submit rules (reported rules report ident (Just call) sender) state ident Message (ConstantText text)
  where
  named = cleanName rules name
  caller = if Text.null named then render rules "caller" [] else named
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

unsubmitted :: Kind -> Receipt
unsubmitted kind = Receipt Failed Nothing (if kind == Outcome then Just True else Nothing)

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
  let replace = maybe True (\job -> kind == Outcome && jobKind job == Message || kind == Message && jobKind job == Outcome && not started) previous
  let outcomeStarted = kind == Message && started && maybe False ((== Outcome) . jobKind) previous
  if stateClosed current || outcomeStarted then pure Nothing else choose current previous replace
  where
  choose _ previous False = pure ((, Nothing) <$> previous)
  choose current previous True
    | statePending current >= messageTaskLimit rules = pure Nothing
    | otherwise = newJob state ident kind current previous

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
  if receiptStatus inherited /= Failed then pure inherited else sendNew
  where
  previousResult = case previous of
    Just earlier | jobKind job == Outcome -> atomically (readTMVar (jobResult earlier))
    _ -> pure (Receipt Failed Nothing Nothing)
  sendNew = do
    when (jobKind job == Outcome) (waitContext rules state ident)
    replacement <- atomically (replacementJob state ident job)
    inherited <- maybe (pure (Receipt Failed Nothing Nothing)) (atomically . readTMVar . jobResult) replacement
    if receiptStatus inherited == Failed then perform else pure inherited
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
  jobs = if matches then Map.delete ident (stateJobs state) else stateJobs state
  contexts = if jobKind job == Outcome && receiptRetryable receipt /= Just True then Map.delete ident (stateContexts state) else stateContexts state

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
      status = case receiptStatus receipt of {Sent -> "sent"; Failed -> "failed"; _ -> "uncertain"} :: Text
  report "incoming_message_delivery" ["call_id" .= call, "event_id" .= ident, "status" .= status]
  pure receipt

normalizeReceipt :: Receipt -> Receipt
normalizeReceipt receipt = receipt {receiptStatus = status, receiptRetryable = retryable}
  where
    status = case receiptStatus receipt of {Skipped -> Uncertain; value -> value}
    retryable = if receiptRetryable receipt == Just True then Just True else Nothing
