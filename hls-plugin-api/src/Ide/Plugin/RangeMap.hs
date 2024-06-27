{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}

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

import           Development.IDE.Graph.Classes            (NFData)

#ifdef USE_FINGERTREE
import           Data.Bifunctor                           (first)
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IM
import           Language.LSP.Protocol.Types              (Position,
                                                           Range (Range))
#else
import           Language.LSP.Protocol.Types              (Range, isSubrangeOf)
#endif

#if USE_FINGERTREE && !MIN_VERSION_base(4,20,0)
import           Data.List                                (foldl')
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
-- NOTE(ozkutuk): In itself, this conversion is wrong. As Michael put it:
-- "LSP Ranges have exclusive upper bounds, whereas the intervals here are
-- supposed to be closed (i.e. inclusive at both ends)"
-- However, in our use-case this turns out not to be an issue (supported
-- by the accompanying property test).  I think the reason for this is,
-- even if rangeToInterval isn't a correct 1:1 conversion by itself, it
-- is used for both the construction of the RangeMap and during the actual
-- filtering (filterByRange), so it still behaves identical to the list
-- approach.
-- This definition isn't exported from the module, therefore we need not
-- worry about other uses where it potentially makes a difference.
rangeToInterval :: Range -> IM.Interval Position
rangeToInterval (Range s e) = IM.Interval s e
#endif
