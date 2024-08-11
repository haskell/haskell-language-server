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
import           Development.IDE.Types.Path
import           GHC.Generics

-- | A mapping of module name to known files
data KnownTargets = KnownTargets
  { targetMap      :: !(HashMap Target (HashSet (Path Abs NormalizedFilePath)))
  -- | 'normalisingMap' is a cached copy of `HMap.mapKey const targetMap`
  --
  -- At startup 'GetLocatedImports' is called on all known files. Say you have 10000
  -- modules in your project then this leads to 10000 calls to 'GetLocatedImports'
  -- running concurrently.
  --
  -- In `GetLocatedImports` the known targets are consulted and the targetsMap
  -- is created by mapping the known targets. This map is used for introducing
  -- sharing amongst filepaths.  This operation copies a local copy of the `target`
  --  map which is local to the rule.
  --
  -- @
  -- let targetsMap = HMap.mapWithKey const targets
  -- @
  --
  -- So now each rule has a 'HashMap' of size 10000 held locally to it and depending
  -- on how the threads are scheduled there will be 10000^2 elements in total
  -- allocated in 'HashMap's. This used a lot of memory.
  --
  -- Solution: Return the 'normalisingMap' in the result of the `GetKnownTargets` rule so it is shared across threads.
  , normalisingMap :: !(HashMap Target Target) } deriving Show


unionKnownTargets :: KnownTargets -> KnownTargets -> KnownTargets
unionKnownTargets (KnownTargets tm nm) (KnownTargets tm' nm') =
  KnownTargets (HMap.unionWith (<>) tm tm') (HMap.union nm nm')

mkKnownTargets :: [(Target, HashSet (Path Abs NormalizedFilePath))] -> KnownTargets
mkKnownTargets vs = KnownTargets (HMap.fromList vs) (HMap.fromList [(k,k) | (k,_) <- vs ])

instance NFData KnownTargets where
  rnf (KnownTargets tm nm) = rnf tm `seq` rnf nm `seq` ()

instance Eq KnownTargets where
  k1 == k2 = targetMap k1 == targetMap k2

instance Hashable KnownTargets where
  hashWithSalt s (KnownTargets hm _) = hashWithSalt s hm

emptyKnownTargets :: KnownTargets
emptyKnownTargets = KnownTargets HMap.empty HMap.empty

data Target = TargetModule ModuleName | TargetFile (Path Abs NormalizedFilePath)
  deriving ( Eq, Ord, Generic, Show )
  deriving anyclass (Hashable, NFData)

toKnownFiles :: KnownTargets -> HashSet (Path Abs NormalizedFilePath)
toKnownFiles = HSet.unions . HMap.elems . targetMap
