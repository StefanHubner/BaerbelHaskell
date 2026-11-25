module Main (main) where

import Baerbel.Core.Config (Settings)
import Baerbel.Core.Error (Failure(..))
import Baerbel.Core.Validation (validateSettings)
import Baerbel.Runtime.Config (loadSettings, readConfig)
import Baerbel.Runtime.HTTP (Send, newSender)
import Data.Aeson (object, (.=))
import LiveCheck.Check (CheckResult(..), emit, successful)
import LiveCheck.Config (Development)
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
run [globalPath, userPath, developmentPath] = configure globalPath userPath developmentPath runSuite
run ["delivery", globalPath, userPath, developmentPath] = configure globalPath userPath developmentPath deliveryChecks
run _ = putStrLn "Usage: baerbel-live-check [delivery] GLOBAL_CONFIG USER_CONFIG DEVELOPMENT_CONFIG" >> exitFailure

configure :: FilePath -> FilePath -> FilePath -> (Development -> Settings -> Send -> IO [CheckResult]) -> IO ()
configure globalPath userPath developmentPath action = do
  loaded <- loadSettings globalPath userPath
  development <- readConfig "development_config" developmentPath
  either rejected execute ((,) <$> development <*> (loaded >>= validateSettings))
  where
    execute (development, settings) = do
      sender <- newSender
      results <- action development settings sender
      finish results

rejected :: Failure -> IO ()
rejected issue = emit detail >> exitFailure
  where detail = object ["configuration_error" .= failureCode issue]

runSuite :: Development -> Settings -> Send -> IO [CheckResult]
runSuite development settings sender = do
  providers <- providerChecks settings sender
  audio <- nativeChecks development settings
  conversations <- conversationChecks development settings sender
  pure (providers ++ audio ++ conversations)

finish :: [CheckResult] -> IO ()
finish results = emit summary >> exit checked
  where
    checked = all successful results
    count status = length (filter ((== status) . checkStatus) results)
    summary = object ["passed" .= count "passed", "failed" .= count "failed", "disabled" .= count "disabled"]
    exit True = pure ()
    exit False = exitFailure
