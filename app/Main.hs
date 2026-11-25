module Main (main) where

import qualified Baerbel.Runtime.Effects as Effects
import Baerbel.Runtime.CLI (runProgram, rejected)
import Control.Monad ((>=>))
import Baerbel.Runtime.ConfigPaths (validatePaths)
import Baerbel.Core.Validation (validateSettings)
import Baerbel.Runtime.Application (checkConfiguration, runApplication)
import Baerbel.Runtime.Config (Settings, loadSettings)
import Baerbel.Core.Error (Result)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = runProgram (getArgs >>= run)

run :: [String] -> IO ()
run ["--check", globalPath, userPath] = configure globalPath userPath check
  where check settings = checkConfiguration settings >>= report "Configuration and native audio checks passed; API access was not tested."
run [globalPath, userPath] = configure globalPath userPath (runApplication >=> report "")
run _ = putStrLn "Usage: phone-assistant-server [--check] GLOBAL_CONFIG USER_CONFIG" >> exitFailure

configure :: FilePath -> FilePath -> (Settings -> IO ()) -> IO ()
configure globalPath userPath action = do
  loaded <- loadSettings globalPath userPath
  checked <- Effects.continue (validatePaths globalPath userPath) (loaded >>= validateSettings)
  either rejected action checked

report :: String -> Result () -> IO ()
report "" = either rejected pure
report success = either rejected (const (putStrLn success))
