module LiveCheck.Delivery.Common (withGoogle, withReceiptLedger, freshId) where

import Baerbel.Core.Config (Settings, rule)
import Baerbel.Core.Error (Result)
import Baerbel.Core.Services (AccountSpec, mutationPolicy)
import Baerbel.Runtime.HTTP (Send, RawSend, authenticated, newRawSender, newSenderWith)
import Baerbel.Runtime.Ledger (Ledger, withLedger)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import LiveCheck.Providers.Google (googleToken)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

withGoogle :: Settings -> Send -> AccountSpec -> (Send -> RawSend -> IO (Result Value)) -> IO (Result Value)
withGoogle settings sender account action = runExceptT $ do
  source <- ExceptT (googleToken settings sender account)
  void (ExceptT source)
  mutations <- except (rule settings "http" "mutations")
  json <- lift (newSenderWith (mutationPolicy mutations))
  raw <- lift newRawSender
  ExceptT (action (authenticated source json) (authenticated source raw))

withReceiptLedger :: (Ledger -> IO (Result a)) -> IO (Result a)
withReceiptLedger action = withSystemTempDirectory "baerbel-delivery-" open
  where open directory = withLedger (directory </> "receipts.sqlite3") action

freshId :: IO Text
freshId = toText <$> nextRandom
