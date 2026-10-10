{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Development.IDE.Types.KnownTargets ( KnownTargets(..)
                                          , emptyKnownTargets
                                          , mkKnownTargets
                                          , mkExtraKnownFiles
                                          , unionKnownTargets
                                          , tombstoneKnownFiles
                                          , Target(..)
                                          , toKnownFiles
                                          , toTargetFiles) where

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

-- | What HLS knows about the files of the workspace
data KnownTargets = KnownTargets
  { targetMap  :: !(HashMap Target (HashSet NormalizedFilePath))
    -- ^ What the session loader discovered: the modules the project is made of
  , knownExtra :: !(HashSet NormalizedFilePath)
    -- ^ Files reported present by the client that no target declares. See
    -- Note [Files that are not targets]
  , knownGone  :: !(HashSet NormalizedFilePath)
    -- ^ Files reported gone by the client. See Note [Tombstones]
  }
  deriving Show

{- Note [Files that are not targets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A workspace often has source files that belong to no session/component like test
fixtures and scratch files. The client reports them like any other and we have
to record them, so that we can maintain an accurate view of the filesystem state
based off which we can run and rerun rules, particularly the ones that resolve
imports like 'GetModulesPaths' and 'GetLocatedImports'.

They are not project modules though, so they are kept apart from the targets.
'toKnownFiles' includes these files, but 'toTargetFiles' does not, because a
file without a component has no session to be compiled in.
-}

{- Note [Tombstones]
~~~~~~~~~~~~~~~~~~~~
'targetMap' is only ever added to: 'extendKnownTargets' unions into it and the
session loader never removes anything. So a file that disappears cannot be
expressed by deleting an entry. Often there is no entry to delete, and deleting
one that is not there leaves 'KnownTargets' equal to what it was, hash and all,
so the early cutoff on 'GetKnownTargets' fires and nothing reruns: the deletion
would be invisible.

Recording the deletion always changes the value, so the rules reading it rerun.
'GetModulesPaths' has to, or an import keeps resolving to a file that is gone,
since 'toKnownFiles' is what tells it about files the file system scan cannot
see. 'GetModuleGraph' has to, or a deleted target stays a root of the graph.

Both 'toKnownFiles' and 'toTargetFiles' hide tombstoned files, which also covers
a target rediscovered by a later cradle load: 'extendKnownTargets' adds the
candidate locations of a 'TargetFile' without checking they exist. A file that
comes back clears its tombstone.
-}

unionKnownTargets :: KnownTargets -> KnownTargets -> KnownTargets
unionKnownTargets (KnownTargets tm extra gone) (KnownTargets tm' extra' gone') =
  KnownTargets (HMap.unionWith (<>) tm tm') (extra <> extra') (gone <> gone')

mkKnownTargets :: [(Target, HashSet NormalizedFilePath)] -> KnownTargets
mkKnownTargets vs = KnownTargets (HMap.fromList vs) HSet.empty HSet.empty

-- | See Note [Files that are not targets]
mkExtraKnownFiles :: HashSet NormalizedFilePath -> KnownTargets
mkExtraKnownFiles fs = KnownTargets HMap.empty fs HSet.empty

-- | Record files as gone, and files that came back as present again.
tombstoneKnownFiles
  :: HashSet NormalizedFilePath -- ^ gone
  -> HashSet NormalizedFilePath -- ^ back
  -> KnownTargets -> KnownTargets
tombstoneKnownFiles gone back kt =
  kt { knownGone = (knownGone kt `HSet.union` gone) `HSet.difference` back }

instance NFData KnownTargets where
  rnf (KnownTargets tm extra gone) = rnf tm `seq` rnf extra `seq` rnf gone `seq` ()

instance Eq KnownTargets where
  k1 == k2 = targetMap k1 == targetMap k2
          && knownExtra k1 == knownExtra k2
          && knownGone k1 == knownGone k2

instance Hashable KnownTargets where
  hashWithSalt s (KnownTargets hm extra gone) =
    hashWithSalt (hashWithSalt (hashWithSalt s hm) (HSet.toList extra)) (HSet.toList gone)

emptyKnownTargets :: KnownTargets
emptyKnownTargets = KnownTargets HMap.empty HSet.empty HSet.empty

data Target = TargetModule ModuleName | TargetFile NormalizedFilePath
  deriving ( Eq, Ord, Generic, Show )
  deriving anyclass (Hashable, NFData)

-- | Every file that is there, as far as we have been told.
toKnownFiles :: KnownTargets -> HashSet NormalizedFilePath
toKnownFiles kt = (targets `HSet.union` knownExtra kt) `HSet.difference` knownGone kt
  where targets = HSet.unions (HMap.elems (targetMap kt))

-- | The files of the project, as declared by the session loader.
-- See Note [Files that are not targets]
toTargetFiles :: KnownTargets -> HashSet NormalizedFilePath
toTargetFiles kt =
  HSet.unions (HMap.elems (targetMap kt)) `HSet.difference` knownGone kt
