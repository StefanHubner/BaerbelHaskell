
module Baerbel.Runtime.MailContent
  ( module Core, decodeWords, decodeCharset ) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Runtime.Effects ( continue )
import Baerbel.Runtime.Error ( ioResult )
import Control.Exception ( evaluate, try )
import Control.Monad.Trans.Except
    ( ExceptT(..), except, runExceptT )
import Data.ByteString ( ByteString )
import Data.Either ( fromRight )
import Data.Text ( Text )
import Data.Text.Encoding.Error ( lenientDecode )
import Data.Text.ICU.Error ( ICUError )
import System.IO ( mkTextEncoding )
import Baerbel.Core.MailContent as Core
    ( HeaderPiece(..),
      Decoder(..),
      htmlText,
      htmlTextWith,
      visible,
      bodyDecoder,
      systemDecoder,
      unicodeName,
      encodingInput,
      nativeUnicode,
      dropPrefix,
      renderPiece,
      parseWords,
      parseWord,
      decodeTransfer,
      quotedPrintable )
import qualified Data.ByteString as Bytes ( useAsCStringLen )
import qualified GHC.Foreign as Foreign ( peekCStringLen )
import qualified Data.Text.ICU.Convert as ICU ( open, toUnicode )
import qualified Data.Text as Text ( concat, pack, unpack )
import qualified Data.Text.Encoding as Text ( decodeUtf8With )

-- Fresh converters stay inside IO: ICU converters carry mutable decoding state.
decodeCharset :: Text -> ByteString -> IO Text
decodeCharset charset bytes = do
  result <- continue decode (bodyDecoder charset bytes)
  either (const (fallbackBody bytes)) pure result

-- ICU groups malformed UTF-8 sequences differently from the lenient text decoder.
decode :: Decoder -> IO (Result Text)
decode (Decoded value) = pure (Right value)
decode (Unicode name bytes) = unicodeBody name bytes
decode (System name bytes) = ioResult "invalid_message_header" convert
  where
    convert = do
      codec <- mkTextEncoding name
      decoded <- Bytes.useAsCStringLen bytes (Foreign.peekCStringLen codec)
      evaluate (Text.pack decoded)

fallbackBody :: ByteString -> IO Text
fallbackBody bytes = do
  result <- unicodeBody "utf-8" bytes
  let fallback = Text.decodeUtf8With lenientDecode bytes
  pure (fromRight fallback result)

unicodeBody :: Text -> ByteString -> IO (Result Text)
unicodeBody name bytes = do
  result <- try (convertUnicode name bytes)
  pure (either failed Right result)
  where
    failed :: ICUError -> Result Text
    failed _ = Left (failure "invalid_message_charset")

convertUnicode :: Text -> ByteString -> IO Text
convertUnicode name bytes = do
  converter <- ICU.open (Text.unpack name) Nothing
  let decoded = ICU.toUnicode converter bytes
  evaluate decoded

decodeWords :: Text -> IO (Result Text)
decodeWords input = runExceptT $ do
  pieces <- except (parseWords input)
  decoded <- traverse (ExceptT . continue decode . renderPiece) pieces
  pure (Text.concat decoded)
