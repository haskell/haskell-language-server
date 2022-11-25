{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A map that allows fast \"in-range\" filtering. 'RangeMap' is meant
-- to be constructed once and cached as part of a Shake rule. If
-- not, the map will be rebuilt upon each invocation, yielding slower
-- results compared to the list-based approach!
--
-- Note that 'RangeMap' falls back to the list-based approach if
-- `use-fingertree` flag of `hls-plugin-api` is set to false.
module Ide.Plugin.RangeMap
  ( RangeMap(..),
    fromList,
    fromList',
    filterByRange,
  ) where

import           Data.Bifunctor                           (first)
import           Data.Foldable                            (foldl')
import           Development.IDE.Graph.Classes            (NFData)
import           Language.LSP.Types                       (Position,
                                                           Range (Range),
                                                           isSubrangeOf)
#ifdef USE_FINGERTREE
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IM
#endif

-- | A map from code ranges to values.
#ifdef USE_FINGERTREE
newtype RangeMap a = RangeMap
  { unRangeMap :: IM.IntervalMap Position a
    -- ^ 'IM.Interval' of 'Position' corresponds to a 'Range'
  }
  deriving newtype (NFData, Semigroup, Monoid)
  deriving stock (Functor, Foldable, Traversable)
#else
newtype RangeMap a = RangeMap
  { unRangeMap :: [(Range, a)] }
  deriving newtype (NFData, Semigroup, Monoid)
  deriving stock (Functor, Foldable, Traversable)
#endif

-- | Construct a 'RangeMap' from a 'Range' accessor and a list of values.
fromList :: (a -> Range) -> [a] -> RangeMap a
fromList extractRange = fromList' . map (\x -> (extractRange x, x))

fromList' :: [(Range, a)] -> RangeMap a
#ifdef USE_FINGERTREE
fromList' = RangeMap . toIntervalMap . map (first rangeToInterval)
  where
    toIntervalMap :: Ord v => [(IM.Interval v, a)] -> IM.IntervalMap v a
    toIntervalMap = foldl' (\m (i, v) -> IM.insert i v m) IM.empty
#else
fromList' = RangeMap
#endif

-- | Filter a 'RangeMap' by a given 'Range'.
filterByRange :: Range -> RangeMap a -> [a]
#ifdef USE_FINGERTREE
filterByRange range = map snd . IM.dominators (rangeToInterval range) . unRangeMap
#else
filterByRange range = map snd . filter (isSubrangeOf range . fst) . unRangeMap
#endif

#ifdef USE_FINGERTREE
rangeToInterval :: Range -> IM.Interval Position
rangeToInterval (Range s e) = IM.Interval s e
#endif