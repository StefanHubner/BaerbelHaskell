{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.CallState
  ( CallState(..), Phase(..), Proposal(..), DialRequest(..), Effect(..)
  , initialCall, proposeRoute, requestDial, announcementFinished, nativeEvent
  , finishCall, requestForward, forwardResult, requestTerminate
  ) where

import Baerbel.Core.AMI (Channel(..), Message, field)
import Baerbel.Core.Error (Result, Failure(..), failure)
import Baerbel.Core.JSON (resultValue)
import Baerbel.Core.Routing (Permission(..), Route(..), Provider(..), routeValue)
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Char (isAlphaNum, isAscii, isDigit)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

data Phase = Conversing | Announcing | Dialing | Connected | RedirectPending | Forwarded | Ended
  deriving (Eq, Show)
data Proposal = Proposal
  { proposalToken :: Text, proposalRoute :: Route, proposalLabel :: Maybe Text, proposalTime :: Double }
  deriving (Eq, Show)
data DialRequest = DialRequest
  { dialRequestId :: Text, dialAttemptId :: Text, dialToken :: Maybe Text
  , dialRoute :: Route, dialLabel :: Maybe Text, dialConfirmed :: Bool
  , dialFreshToken :: Text, dialTime :: Double
  } deriving (Eq, Show)
data CallState = CallState
  { callId :: Text, callPermission :: Permission, callChannel :: Channel, callPhase :: Phase
  , callProposal :: Maybe Proposal, callRequest :: Maybe DialRequest
  , callControlLost :: Bool, callResults :: Map Text Value
  } deriving (Eq, Show)
data Effect
  = Announce Route (Maybe Text) | DialNative Route Text | ResumeAssistant
  | ReleaseAssistant | HangupChannel Text | RedirectChannel Text Text
  deriving (Eq, Show)

initialCall :: Text -> Permission -> Channel -> CallState
initialCall identifier permission channel = CallState identifier permission channel Conversing Nothing Nothing False M.empty

proposeRoute :: Double -> Text -> Route -> Maybe Text -> CallState -> (CallState, Value)
proposeRoute now token route label state = (updated, routeValue token route label)
  where
    updated :: CallState
    updated = state { callProposal = Just (Proposal token route label now) }

proposalMatches :: DialRequest -> Maybe Proposal -> Bool
proposalMatches _ Nothing = False
proposalMatches request (Just proposal) = dialToken request == Just (proposalToken proposal)
  && dialRoute request == proposalRoute proposal && dialLabel request == proposalLabel proposal
  && dialTime request - proposalTime proposal <= 300

requiresProposal :: DialRequest -> CallState -> Bool
requiresProposal request state = home && not matches || international && (not matches || not confirmed)
  || isJust (dialToken request) && not matches
  where
    home :: Bool
    home = routeProvider (dialRoute request) == Home
    international :: Bool
    international = routeConfirmation (dialRoute request)
    confirmed :: Bool
    confirmed = dialConfirmed request
    matches :: Bool
    matches = proposalMatches request (callProposal state)

requestDial :: Bool -> DialRequest -> CallState -> Result (CallState, [Effect], Maybe Value)
requestDial enabled request state
  | callPermission state /= Owner = Left (failure "owner_call_required")
  | Just result <- M.lookup identifier (callResults state) = Right (state, [], Just result)
  | callPhase state == Ended = Right (state, [], Just (resultValue "cancelled"))
  | callPhase state == Connected = Right (state, [], Just (resultValue "connected"))
  | not enabled = Left (failure "direct_calls_disabled")
  | callPhase state /= Conversing = Left (failure "call_already_in_progress")
  | requiresProposal request state = Right (proposed, [], Just proposalResult)
  | M.size (callResults state) >= 64 = Left (failure "call_attempt_limit")
  | otherwise = Right (updated, [Announce (dialRoute request) (dialLabel request)], Nothing)
  where
    identifier :: Text
    identifier = dialRequestId request
    proposed :: CallState
    proposalResult :: Value
    (proposed, proposalResult) = proposeRoute (dialTime request) (dialFreshToken request) (dialRoute request) (dialLabel request) state
    updated :: CallState
    updated = state { callProposal = Nothing, callRequest = Just request, callPhase = Announcing }

remember :: Value -> CallState -> CallState
remember result state = case callRequest state of
  Nothing -> state
  Just request -> state { callResults = M.insert (dialRequestId request) result (callResults state) }

announcementFinished :: Bool -> Bool -> CallState -> Result (CallState, [Effect])
announcementFinished _ _ state | callPhase state /= Announcing = Right (state, [])
announcementFinished False _ state = Right (cancelled, [])
  where
    result :: Value
    result = object ["status" .= ("ok" :: Text), "result" .= ("cancelled" :: Text), "reason" .= ("owner_interrupted_before_dial" :: Text)]
    cancelled :: CallState
    cancelled = (remember result state) { callPhase = Conversing }
announcementFinished True False _ = Left (failure "owner_call_required")
announcementFinished True True state = case callRequest state of
  Nothing -> Left (failure "call_attempt_missing")
  Just request -> Right (state { callPhase = Dialing }, [DialNative (dialRoute request) (dialAttemptId request)])

nativeEvent :: Message -> CallState -> (CallState, [Effect])
nativeEvent message state
  | callPhase state == Ended = (state, [])
  | hangup = (remember (resultValue "cancelled") state { callPhase = Ended }, [])
  | field "Event" message == "AMIConnectionClosed" = controlLost state
  | not matching = (state, [])
  | field "UserEvent" message == "PhoneAssistantDialAnswered" = (remember (resultValue "connected") state { callPhase = Connected }, [ReleaseAssistant])
  | field "UserEvent" message == "PhoneAssistantDialResult" && callPhase state /= Connected = nativeResult message state
  | otherwise = (state, [])
  where
    user :: Bool
    user = field "Event" message == "UserEvent" && field "CallID" message == callId state
    hangup :: Bool
    hangup = field "Event" message == "Hangup" && field "Uniqueid" message == channelUniqueId (callChannel state)
      || user && field "UserEvent" message == "PhoneAssistantHangup"
    matching :: Bool
    matching = user && maybe False (\request -> field "AttemptID" message == dialAttemptId request) (callRequest state)

controlLost :: CallState -> (CallState, [Effect])
controlLost state = (updated {callControlLost = True}, effects)
  where
    result :: Value
    result = object ["status" .= ("ok" :: Text), "result" .= ("failed" :: Text), "reason" .= ("call_control_disconnected" :: Text)]
    updated :: CallState
    effects :: [Effect]
    (updated, effects) = finishResult result state

nativeResult :: Message -> CallState -> (CallState, [Effect])
nativeResult message = finishResult result
  where
    cause :: Maybe Int
    cause = parseCause (field "Cause" message)
    sip :: Maybe Int
    sip = parseSip (field "SIPCause" message)
    outcome :: Text
    outcome
      | cause == Just 21 || sip `elem` [Just 403, Just 603] = "rejected"
      | otherwise = M.findWithDefault "failed" (field "Result" message) outcomes
    outcomes :: Map Text Text
    outcomes = M.fromList [("BUSY", "busy"), ("NOANSWER", "no_answer"), ("CANCEL", "cancelled"), ("INVALIDARGS", "rejected"), ("DONTCALL", "rejected")]
    result :: Value
    result = object ["status" .= ("ok" :: Text), "result" .= outcome, "cause" .= cause, "sip_status" .= sip]

parseCause :: Text -> Maybe Int
parseCause text
  | T.null text || not (T.all isDigit text) = Nothing
  | otherwise = readMaybe (T.unpack text) >>= inRange
  where
    inRange :: Int -> Maybe Int
    inRange value
      | value >= 0 && value <= 127 = Just value
      | otherwise = Nothing

parseSip :: Text -> Maybe Int
parseSip text = listToMaybe [value | index <- [0 .. T.length text - 3], valid index,
  Just value <- [readMaybe (T.unpack (T.take 3 (T.drop index text)))]]
  where
    word :: Char -> Bool
    word char = isAlphaNum char || char == '_'
    boundary :: Text -> Bool
    boundary value = maybe True (not . word . fst) (T.uncons value)
    valid :: Int -> Bool
    valid index = boundary (T.reverse (T.take index text)) && boundary (T.drop (index + 3) text)
      && maybe False (\(char, _) -> char >= '3' && char <= '6') (T.uncons (T.drop index text))
      && T.all (\char -> isAscii char && isDigit char) (T.take 3 (T.drop index text))

finishResult :: Value -> CallState -> (CallState, [Effect])
finishResult result state
  | callPhase state /= Dialing = (state, [])
  | otherwise = ((remember result state) {callPhase = Conversing}, [ResumeAssistant])

finishCall :: Bool -> Bool -> Bool -> CallState -> Result (CallState, [Effect])
finishCall explicit pending farewellInterrupted state
  | callPermission state /= Owner || callPhase state /= Conversing = Left (failure "end_call_unavailable")
  | not explicit && pending = Left (failure "finish_pending_work_before_ending_call")
  | farewellInterrupted = Right (state, [])
  | otherwise = Right (state { callPhase = Ended }, [HangupChannel (channelName (callChannel state))])

requestForward :: Bool -> CallState -> Result (CallState, [Effect])
requestForward owned state
  | callPermission state /= Screening = Left (failure "incoming_call_not_owned")
  | channelMessageOnly (callChannel state) = Left (failure "incoming_message_only")
  | callPhase state `elem` [RedirectPending, Forwarded, Ended] = Right (state, [])
  | not owned = Left (failure "incoming_call_not_owned")
  | otherwise = Right (state { callPhase = RedirectPending }, [RedirectChannel (channelName (callChannel state)) "phone-assistant-screened-forward"])

forwardResult :: Result a -> CallState -> CallState
forwardResult (Right _) state = state { callPhase = Forwarded }
forwardResult (Left problem) state
  | failureCode problem == "ami_action_rejected" = state { callPhase = Conversing }
  | otherwise = state

requestTerminate :: Bool -> CallState -> (CallState, [Effect])
requestTerminate owned state
  | callPhase state `elem` [RedirectPending, Forwarded, Ended] = (state, [])
  | owned = (state { callPhase = Ended }, [HangupChannel (channelName (callChannel state))])
  | otherwise = (state { callPhase = Ended }, [])
