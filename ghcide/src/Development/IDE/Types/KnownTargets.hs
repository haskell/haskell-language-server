{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Development.IDE.Types.KnownTargets (KnownTargets, Target(..), toKnownFiles) where

import           Control.DeepSeq
import           Data.Hashable
import           Data.HashMap.Strict
import qualified Data.HashMap.Strict            as HMap
import           Data.HashSet
import qualified Data.HashSet                   as HSet
import           Development.IDE.GHC.Compat     (ModuleName)
import           Development.IDE.GHC.Orphans    ()
import           Development.IDE.Types.Location
import           GHC.Generics

-- | A mapping of module name to known files
type KnownTargets = HashMap Target (HashSet NormalizedFilePath)

data Target = TargetModule ModuleName | TargetFile NormalizedFilePath
  deriving ( Eq, Generic, Show )
  deriving anyclass (Hashable, NFData)

toKnownFiles :: KnownTargets -> HashSet NormalizedFilePath
toKnownFiles = HSet.unions . HMap.elems
