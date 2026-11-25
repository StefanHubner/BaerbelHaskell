module Main (main) where

import Baerbel.Core.Config (Settings)
import Baerbel.Core.Error (Failure(..))
import Baerbel.Core.Validation (validateSettings)
import Baerbel.Runtime.Config (loadSettings)
import Baerbel.Runtime.HTTP (Send, newSender)
import Data.Aeson (object, (.=))
import LiveCheck.Check (CheckResult(..), emit, successful)
import LiveCheck.Conversation (conversationChecks)
import LiveCheck.Delivery (deliveryChecks)
import LiveCheck.Native (nativeChecks)
import LiveCheck.Providers (providerChecks)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = hSetBuffering stdout LineBuffering >> getArgs >>= run

run :: [String] -> IO ()
run [globalPath, userPath] = configure globalPath userPath runSuite
run ["delivery", globalPath, userPath] = configure globalPath userPath deliveryChecks
run _ = putStrLn "Usage: baerbel-live-check [delivery] GLOBAL_CONFIG USER_CONFIG" >> exitFailure

configure :: FilePath -> FilePath -> (Settings -> Send -> IO [CheckResult]) -> IO ()
configure globalPath userPath action = do
  loaded <- loadSettings globalPath userPath
  either rejected execute (loaded >>= validateSettings)
  where
    execute settings = do
      sender <- newSender
      results <- action settings sender
      finish results

rejected :: Failure -> IO ()
rejected issue = emit detail >> exitFailure
  where detail = object ["configuration_error" .= failureCode issue]

runSuite :: Settings -> Send -> IO [CheckResult]
runSuite settings sender = do
  providers <- providerChecks settings sender
  audio <- nativeChecks settings
  conversations <- conversationChecks settings sender
  pure (providers ++ audio ++ conversations)

finish :: [CheckResult] -> IO ()
finish results = emit summary >> exit checked
  where
    checked = all successful results
    count status = length (filter ((== status) . checkStatus) results)
    summary = object ["passed" .= count "passed", "failed" .= count "failed", "disabled" .= count "disabled"]
    exit True = pure ()
    exit False = exitFailure
