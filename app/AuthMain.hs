module Main (main) where

import qualified Baerbel.Effects as Effects
import Baerbel.CLI (runProgram, rejected)
import Baerbel.Authorize
import Baerbel.Config (Global, User(..), Settings, resolveSettings, rule)
import Baerbel.Error (Result, ioResult)
import Baerbel.HTTP (newRawSender)
import Baerbel.JSON (decodeBytes)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Process (CreateProcess(..), StdStream(NoStream), createProcess, proc)

main :: IO ()
main = runProgram (getArgs >>= run)

run :: [String] -> IO ()
run (globalPath:arguments) = do
  raw <- ioResult "global_config_unavailable" (BS.readFile globalPath)
  let prepared = raw >>= decodeBytes "global_config_invalid" >>= authorizationSettings
  result <- Effects.continue (configure arguments) prepared
  either rejected (const (pure ())) result
run _ = Text.hPutStrLn stderr "Usage: phone-assistant-gmail-auth GLOBAL_CONFIG --client-secrets FILE [--token-file FILE] [--calendar] [--contacts] [--no-gmail] [--no-browser] [--port PORT] [--timeout SECONDS]" >> exitFailure

authorizationSettings :: Global -> Result (AuthorizationRules, ScopeGroups)
authorizationSettings global = do
  rules <- rule settings "oauth" "authorization"
  gmail <- rule settings "oauth" "gmail"
  calendar <- rule settings "oauth" "calendar"
  contacts <- rule settings "oauth" "contacts"
  pure (rules, Map.fromList [("gmail", gmail), ("calendar", calendar), ("contacts", contacts)])
  where
    settings :: Settings
    settings = resolveSettings global (User Map.empty)

configure :: [String] -> (AuthorizationRules, ScopeGroups) -> IO (Result ())
configure arguments (rules, groups) = Effects.continue open (parseOptions rules arguments)
  where
    open :: AuthorizationOptions -> IO (Result ())
    open options = do
      client <- readClient rules (authClientFile options)
      sender <- newRawSender
      let effects = AuthorizationEffects sender Text.putStrLn openBrowser getCurrentTime
      Effects.continue (\value -> authorize rules groups options value effects) client

openBrowser :: Text -> IO ()
openBrowser url = void (ioResult "authorization_browser_unavailable" (createProcess command))
  where
    command :: CreateProcess
    command = (proc "xdg-open" [Text.unpack url]) {std_in = NoStream, std_out = NoStream, std_err = NoStream}
