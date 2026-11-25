module Baerbel.Runtime.ConfigPaths (validatePaths) where

import Baerbel.Core.ConfigPaths
    ( PathCheck(checkDistinct, checkFile),
      PathRule,
      preparePaths,
      pathResult )
import Baerbel.Runtime.Config ( Settings, rule )
import Baerbel.Runtime.Error ( Result, ioResult )
import System.Directory
    ( canonicalizePath, doesFileExist, getHomeDirectory )
import qualified Baerbel.Runtime.Effects as Effects ( continue )

validatePaths :: FilePath -> FilePath -> Settings -> IO (Result Settings)
validatePaths globalPath userPath settings = Effects.continue inspect configured
  where
    configured = rule settings "validation" "paths"
    inspect :: [PathRule] -> IO (Result Settings)
    inspect rules = do
      home <- getHomeDirectory
      let (prepared, checks) = preparePaths globalPath userPath home settings rules
          observations = traverse (Effects.continue inspectPath) checks
      result <- ioResult "configuration_path_unavailable" observations
      pure (prepared <$ (result >>= sequence_))

inspectPath :: PathCheck -> IO (Result ())
inspectPath check = do
  exists <- doesFileExist (checkFile check)
  target <- canonicalizePath (checkFile check)
  others <- traverse canonicalizePath (checkDistinct check)
  pure (pathResult check exists target others)
