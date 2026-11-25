module Main (main) where

import Baerbel.Core.Accounts (Subscriber)
import Baerbel.Core.Config (Settings)
import Baerbel.Core.Error (Result)
import Baerbel.Runtime.Accounts (issuePin, connectGoogle, loadInstallation)
import Baerbel.Runtime.Application (checkAccounts, checkConfiguration, runAccounts, runApplication)
import Baerbel.Runtime.CLI (runProgram, rejected)
import Baerbel.Runtime.Effects (continue)
import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = runProgram (getArgs >>= run)

run :: [String] -> IO ()
run ["--check", path] = check path Nothing
run ["--check", globalPath, localPath] = check globalPath (Just localPath)
run ["--issue-pin", path, ident] = issuePin path (pack ident) >>= report "PIN saved and SMS accepted."
run ["--connect-google", path, ident, service, token] =
  connectGoogle path (pack ident) (pack service) token >>= report "Google connection saved. Restart Bärbel to apply it."
run [path] = start path Nothing
run [globalPath, localPath] = start globalPath (Just localPath)
run _ = putStrLn "Usage: phone-assistant-server [--check] CONFIG | --issue-pin CONFIG ACCOUNT | --connect-google CONFIG ACCOUNT SERVICE TOKEN_FILE" >> exitFailure

check :: FilePath -> Maybe FilePath -> IO ()
check path local = configure path local inspect >>= report "Configuration and native audio checks passed; API access was not tested."
  where inspect = either checkConfiguration (\(_, database, accounts) -> checkAccounts database accounts)

start :: FilePath -> Maybe FilePath -> IO ()
start path local = configure path local open >>= report ""
  where open = either runApplication (\(settings, database, accounts) -> runAccounts settings database accounts)

configure :: FilePath -> Maybe FilePath -> (Either Settings (Settings, FilePath, Map Text Subscriber) -> IO (Result ())) -> IO (Result ())
configure path local action = loadInstallation path local >>= continue action

report :: String -> Result () -> IO ()
report "" = either rejected pure
report success = either rejected (const (putStrLn success))
