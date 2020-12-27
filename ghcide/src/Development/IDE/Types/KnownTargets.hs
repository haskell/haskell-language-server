{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Development.IDE.Types.KnownTargets (KnownTargets, Target(..), toKnownFiles) where

import Data.HashMap.Strict
import Development.IDE.Types.Location
import Development.IDE.GHC.Compat (ModuleName)
import Development.IDE.GHC.Orphans ()
import Data.Hashable
import GHC.Generics
import Control.DeepSeq
import Data.HashSet
import qualified Data.HashSet as HSet
import qualified Data.HashMap.Strict as HMap

-- | A mapping of module name to known files
type KnownTargets = HashMap Target [NormalizedFilePath]

data Target = TargetModule ModuleName | TargetFile NormalizedFilePath
  deriving ( Eq, Generic, Show )
  deriving anyclass (Hashable, NFData)

toKnownFiles :: KnownTargets -> HashSet NormalizedFilePath
toKnownFiles = HSet.fromList . concat . HMap.elems
