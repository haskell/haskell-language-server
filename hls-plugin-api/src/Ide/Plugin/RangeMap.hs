{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A map that allows fast \"in-range\" filtering. 'RangeMap' is meant
-- to be constructed once and cached as part of a Shake rule. If
-- not, the map will be rebuilt upon each invocation, yielding slower
-- results compared to the list-based approach!
module Ide.Plugin.RangeMap
  ( RangeMap(..),
    fromList,
    fromList',
    filterByRange,
  ) where

import           Data.Bifunctor                           (first)
import           Data.Foldable                            (foldl')
import           Development.IDE.Graph.Classes            (NFData)
import qualified HaskellWorks.Data.IntervalMap.FingerTree as IM
import           Language.LSP.Types                       (Position,
                                                           Range (Range))

-- | A map from code ranges to values.
newtype RangeMap a = RangeMap
  { unRangeMap :: IM.IntervalMap Position a
    -- ^ 'IM.Interval' of 'Position' corresponds to a 'Range'
  }
  deriving newtype (NFData)

-- | Construct a 'RangeMap' from a 'Range' accessor and a list of values.
fromList :: (a -> Range) -> [a] -> RangeMap a
fromList extractRange = fromList' . map (\x -> (extractRange x, x))

fromList' :: [(Range, a)] -> RangeMap a
fromList' = RangeMap . toIntervalMap . map (first rangeToInterval)
  where
    toIntervalMap :: Ord v => [(IM.Interval v, a)] -> IM.IntervalMap v a
    toIntervalMap = foldl' (\m (i, v) -> IM.insert i v m) IM.empty

-- | Filter a 'RangeMap' by a given 'Range'.
filterByRange :: Range -> RangeMap a -> [a]
filterByRange range = map snd . IM.dominators (rangeToInterval range) . unRangeMap

rangeToInterval :: Range -> IM.Interval Position
rangeToInterval (Range s e) = IM.Interval s e
