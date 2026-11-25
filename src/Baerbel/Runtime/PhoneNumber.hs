{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.PhoneNumber (validateNumber) where

import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.Routing ( normalizeDestination )
import Data.Text ( Text )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt(..) )
import qualified Data.Text as T ( unpack )

foreign import ccall safe "baerbel_validate_number" nativeValidate :: CString -> IO CInt

-- Parsing and classification belong to the maintained upstream library.
-- The IO boundary owns its internal metadata caches; no unsafePerformIO.
validateNumber :: Text -> IO (Result Text)
validateNumber input = case normalizeDestination input of
  Left problem -> pure (Left problem)
  Right canonical -> withCString (T.unpack canonical) (validate canonical)

validate :: Text -> CString -> IO (Result Text)
validate canonical pointer = classify canonical <$> nativeValidate pointer

classify :: Text -> CInt -> Result Text
classify canonical 0 = Right canonical
classify _ 2 = Left (failure "destination_class_not_permitted")
classify _ _ = Left (failure "invalid_e164_number")
