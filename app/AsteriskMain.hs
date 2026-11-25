module Main (main) where

import Baerbel.Core.Error (Result, failure)
import Baerbel.Runtime.Asterisk (InstallStatus(..), adopt, install, preview, restore)
import Baerbel.Runtime.CLI (rejected, runProgram)
import System.Environment (getArgs)

main :: IO ()
main = runProgram (getArgs >>= run >>= either rejected pure)

run :: [String] -> IO (Result ())
run ["adopt", root, routes, config, database] = adopt root routes config database
run ["prepare", config, database, destination] = preview config database destination
run ["install", config, database, backup] = do
  result <- install config database backup
  either (pure . Left) report result
run ["restore", root, backup] = restore root backup
run _ = pure (Left (failure "usage: asteriskconfig adopt ROOT ROUTES CONFIG DB | prepare CONFIG DB DIRECTORY | install CONFIG DB BACKUP | restore ROOT BACKUP"))

report :: InstallStatus -> IO (Result ())
report Current = putStrLn "Asterisk configuration already current." >> pure (Right ())
report Updated = putStrLn "Asterisk configuration updated." >> pure (Right ())
