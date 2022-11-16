{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

newtype RangeMap a
  = RangeMap { unRangeMap :: IM.IntervalMap Position a }
  deriving newtype (NFData)

rangeToInterval :: Range -> IM.Interval Position
rangeToInterval (Range s e) = IM.Interval s e

fromList :: (a -> Range) -> [a] -> RangeMap a
fromList extractRange = fromList' . map (\x -> (extractRange x, x))

fromList' :: [(Range, a)] -> RangeMap a
fromList' = RangeMap . toIntervalMap . map (first rangeToInterval)
  where
    toIntervalMap :: Ord v => [(IM.Interval v, a)] -> IM.IntervalMap v a
    toIntervalMap = foldl' (\m (i, v) -> IM.insert i v m) IM.empty

filterByRange :: Range -> RangeMap a -> [a]
filterByRange range = map snd . IM.dominators (rangeToInterval range) . unRangeMap
