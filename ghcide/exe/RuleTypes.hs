{-# LANGUAGE TypeFamilies #-}
module RuleTypes (GetHscEnv(..), LoadCradle(..)) where

import Control.DeepSeq
import Data.Binary
import Data.Hashable (Hashable)
import Development.Shake
import Development.IDE.GHC.Util
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- Rule type for caching GHC sessions.
type instance RuleResult GetHscEnv = HscEnvEq

data GetHscEnv = GetHscEnv
    { hscenvOptions :: [String]        -- componentOptions from hie-bios
    , hscenvRoot :: FilePath           -- componentRoot from hie-bios
    , hscenvDependencies :: [FilePath] -- componentDependencies from hie-bios
    }
    deriving (Eq, Show, Typeable, Generic)

instance Hashable GetHscEnv
instance NFData   GetHscEnv
instance Binary   GetHscEnv

-- Rule type for caching cradle loading
type instance RuleResult LoadCradle = HscEnvEq

data LoadCradle = LoadCradle
    deriving (Eq, Show, Typeable, Generic)

instance Hashable LoadCradle
instance NFData   LoadCradle
instance Binary   LoadCradle
