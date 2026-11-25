{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Pin where

import Baerbel.Core.Error (Result, failure)
import Crypto.Hash (SHA256(..))
import Crypto.KDF.PBKDF2 (Parameters(..), generate, prfHMAC)
import Data.Aeson (Value)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base(Base16), convertFromBase)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data PinConfig = PinConfig
  { pinEncoded :: Text, pinPrefixes :: [Text], pinWindowMicros :: Int
  , pinMinimumDigits :: Int, pinMaximumDigits :: Int, pinPrompt :: Value, pinIncorrect :: Value
  }

data PinHash = PinHash Int BS.ByteString BS.ByteString

data PinState = PinState
  { pinDigits :: BS.ByteString, pinOverflow :: Bool, pinUnlocked :: Bool
  } deriving (Eq, Show)

emptyPinState :: PinState
emptyPinState = PinState BS.empty False False

parsePinHash :: Text -> Result PinHash
parsePinHash encoded = case T.splitOn ":" encoded of
  ["pbkdf2_sha256", "600000", saltText, expectedText]
    | T.length saltText == 32 && T.length expectedText == 64
      && T.all lowerHex (saltText <> expectedText) ->
      PinHash 600000 <$> decodeHex saltText <*> decodeHex expectedText
  _ -> Left (failure "invalid_private_pin_hash")
  where
    lowerHex char = char `elem` ("0123456789abcdef" :: String)

decodeHex :: Text -> Result BS.ByteString
decodeHex text = either (const (Left (failure "invalid_private_pin_hash"))) Right decoded
  where
    decoded = convertFromBase Base16 (TE.encodeUtf8 text)

verifyPin :: PinHash -> BS.ByteString -> Bool
verifyPin (PinHash rounds salt expected) digits = constEq actual expected
  where
    parameters = Parameters rounds 32
    actual = generate (prfHMAC SHA256) parameters digits salt :: BS.ByteString

keypadStep :: Int -> BS.ByteString -> PinState -> (PinState, Maybe BS.ByteString)
keypadStep limit digit state
  | pinUnlocked state = (state, Nothing)
  | digit == "*" = (emptyPinState, Nothing)
  | digit == "#" = (emptyPinState, Just submitted)
  | BS.length digit /= 1 || digit < "0" || digit > "9" = (state, Nothing)
  | BS.length (pinDigits state) >= limit = (state { pinOverflow = True }, Nothing)
  | otherwise = (state { pinDigits = pinDigits state <> digit }, Nothing)
  where
    submitted | pinOverflow state = BS.empty
              | otherwise = pinDigits state

verifiedState :: Integer -> Bool -> Bool -> (PinState, Integer)
verifiedState renewed verified stopped = (emptyPinState {pinUnlocked = verified && not stopped}, expires)
  where
    expires | verified || stopped = 0
            | otherwise = renewed
