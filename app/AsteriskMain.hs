module Main (main) where

import Baerbel.Core.Error (Result, failure)
import Baerbel.Runtime.Asterisk (InstallStatus(..), adopt, install, installSite, preview, previewSite, restore)
import Baerbel.Runtime.CLI (rejected, runProgram)
import System.Environment (getArgs)

main :: IO ()
main = runProgram (getArgs >>= run >>= either rejected pure)

run :: [String] -> IO (Result ())
run ["adopt", root, routes, config, database] = adopt root routes config database
run ["prepare", config, database, destination] = preview config database destination
run ["prepare", config, database, templates, destination] = previewSite config database templates destination
run ["install", config, database, backup] = install config database backup >>= either (pure . Left) report
run ["install", config, database, templates, backup] = installSite config database templates backup >>= either (pure . Left) report
run ["restore", root, backup] = restore root backup
run _ = pure (Left (failure "usage: asteriskconfig adopt ROOT ROUTES CONFIG DB | prepare CONFIG DB TEMPLATES DIRECTORY | install CONFIG DB TEMPLATES BACKUP | restore ROOT BACKUP"))

report :: InstallStatus -> IO (Result ())
report Current = putStrLn "Asterisk configuration already current." >> pure (Right ())
report Updated = putStrLn "Asterisk configuration updated." >> pure (Right ())
