module Main (main) where

import Baerbel.Core.Accounts (Subscriber)
import Baerbel.Core.Config (Settings)
import Baerbel.Core.Error (Result)
import Baerbel.Runtime.Accounts (issuePin, loadInstallation)
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
run ["--check", globalPath, userPath] = configure globalPath userPath check >>= report "Configuration and native audio checks passed; API access was not tested."
  where check = either checkConfiguration (checkAccounts . snd)
run ["--issue-pin", globalPath, userPath, ident] = issuePin globalPath userPath (pack ident) >>= report "PIN saved and SMS accepted."
run [globalPath, userPath] = configure globalPath userPath start >>= report ""
  where start = either runApplication (uncurry runAccounts)
run _ = putStrLn "Usage: phone-assistant-server [--check] GLOBAL_CONFIG USER_CONFIG | --issue-pin GLOBAL_CONFIG USER_CONFIG ACCOUNT" >> exitFailure

configure :: FilePath -> FilePath -> (Either Settings (Settings, Map Text Subscriber) -> IO (Result ())) -> IO (Result ())
configure globalPath userPath action = loadInstallation globalPath userPath >>= continue action

report :: String -> Result () -> IO ()
report "" = either rejected pure
report success = either rejected (const (putStrLn success))
