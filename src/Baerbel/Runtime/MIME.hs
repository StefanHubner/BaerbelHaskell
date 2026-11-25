
module Baerbel.Runtime.MIME
  ( module Core, render ) where

import Data.ByteString ( ByteString )
import Baerbel.Core.MIME as Core
    ( File(..), Envelope(..), compose )
import qualified Data.ByteString.Lazy as Lazy ( toStrict )
import qualified Network.Mail.Mime as MIME ( renderMail' )

render :: Envelope -> [File] -> IO ByteString
render envelope files = fmap Lazy.toStrict (MIME.renderMail' message)
  where message = compose envelope files
