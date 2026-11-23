module Main (main) where

import Distribution.PackageDescription (GenericPackageDescription, HookedBuildInfo)
import Distribution.Simple (UserHooks(..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (ConfigFlags(..))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {confHook = configureNative}

configureNative :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
configureNative description flags = do
  root <- getCurrentDirectory
  let directory = root </> ".native" </> "lib"
      configured = flags {configExtraLibDirs = directory : configExtraLibDirs flags}
  confHook simpleUserHooks description configured
