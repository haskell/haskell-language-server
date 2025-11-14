{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Development.IDE.Types.KnownTargets ( KnownTargets(..)
                                          , emptyKnownTargets
                                          , mkKnownTargets
                                          , unionKnownTargets
                                          , Target(..)
                                          , toKnownFiles) where

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
newtype KnownTargets = KnownTargets
  { targetMap :: (HashMap Target (HashSet NormalizedFilePath)) }
  deriving Show


unionKnownTargets :: KnownTargets -> KnownTargets -> KnownTargets
unionKnownTargets (KnownTargets tm) (KnownTargets tm') =
  KnownTargets (HMap.unionWith (<>) tm tm')

mkKnownTargets :: [(Target, HashSet NormalizedFilePath)] -> KnownTargets
mkKnownTargets vs = KnownTargets (HMap.fromList vs)

instance NFData KnownTargets where
  rnf (KnownTargets tm) = rnf tm `seq` ()

instance Eq KnownTargets where
  k1 == k2 = targetMap k1 == targetMap k2

instance Hashable KnownTargets where
  hashWithSalt s (KnownTargets hm) = hashWithSalt s hm

emptyKnownTargets :: KnownTargets
emptyKnownTargets = KnownTargets HMap.empty

data Target = TargetModule ModuleName | TargetFile NormalizedFilePath
  deriving ( Eq, Ord, Generic, Show )
  deriving anyclass (Hashable, NFData)

toKnownFiles :: KnownTargets -> HashSet NormalizedFilePath
toKnownFiles = HSet.unions . HMap.elems . targetMap
