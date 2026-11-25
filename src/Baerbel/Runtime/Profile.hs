module Baerbel.Runtime.Profile (ProfileState(..), ProfileStore(..), Command(..), profile, parseCommand, transition, emptyProfile, snapshot, validKey, validValue) where

import Baerbel.Core.Profile
    ( Command(..),
      ProfileState(..),
      emptyProfile,
      validKey,
      validValue,
      snapshot,
      parseCommand,
      transition )
import Baerbel.Core.ProviderConfig ( ProfileRules(..) )
import Baerbel.Runtime.Error ( Result )
import Baerbel.Runtime.Provider ( Execute )
import Data.Aeson ( Value )
import Data.Time ( UTCTime )
import qualified Baerbel.Runtime.Effects as Effects ( continue )

data ProfileStore = ProfileStore
  { profileRead :: IO (Result ProfileState)
  , profileUpdate :: (ProfileState -> Result (ProfileState, Value)) -> IO (Result Value)
  }

profile :: ProfileRules -> ProfileStore -> IO UTCTime -> Execute
profile rules store clock name arguments _ = Effects.continue run prepared
  where
    prepared = parseCommand rules name arguments
    run (ReadProfile key) = fmap (\state -> snapshot rules state key) <$> profileRead store
    run command = do
      now <- clock
      profileUpdate store (transition rules now command)
