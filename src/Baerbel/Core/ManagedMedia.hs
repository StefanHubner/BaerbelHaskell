{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.ManagedMedia where

import Baerbel.Core.AudioSocket (Packet(..), PacketKind(..))
import Baerbel.Core.Error (Result, failure)
import Data.ByteString (ByteString)

data PacketAction = ReceiveNext | AwaitResume Int | DeliverAudio (Maybe ByteString) | DeliverDigit ByteString
disconnected :: Result Packet -> Bool
disconnected (Left _) = True
disconnected (Right (Packet Hangup _)) = True
disconnected _ = False

handlePacket :: Result Packet -> Result PacketAction
handlePacket (Left problem)
  | problem == failure "audiosocket_incomplete_read" = Right (DeliverAudio Nothing)
  | otherwise = Left problem
handlePacket (Right (Packet Hangup _)) = Right (DeliverAudio Nothing)
handlePacket (Right (Packet Audio pcm)) = Right (DeliverAudio (Just pcm))
handlePacket (Right (Packet Dtmf digit)) = Right (DeliverDigit digit)
handlePacket (Right _) = Left (failure "audiosocket_unexpected_packet")
