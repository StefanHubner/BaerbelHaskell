module Baerbel.Runtime.CLI (runProgram, rejected) where

import Baerbel.Runtime.Error (Failure, failureCode, ioResult)
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

runProgram :: IO () -> IO ()
runProgram action = ioResult "application_io_failed" action >>= either rejected pure

rejected :: Failure -> IO ()
rejected issue = Text.hPutStrLn stderr (failureCode issue) >> exitFailure
