-- A benchmark comparing the performance characteristics of list-based
-- vs RangeMap-based "in-range filtering" approaches
module Main (main) where

import           Control.DeepSeq        (force)
import           Control.Exception      (evaluate)
import           Control.Monad          (replicateM)
import qualified Criterion
import qualified Criterion.Main
import           Data.Random            (RVar)
import qualified Data.Random            as Fu
import qualified Ide.Plugin.RangeMap    as RangeMap
import           Language.LSP.Types     (Position (..), Range (..), UInt,
                                         isSubrangeOf)
import qualified System.Random.Stateful as Random


genRangeList :: Int -> RVar [Range]
genRangeList n = replicateM n genRange

genRange :: RVar Range
genRange = do
  x1 <- genPosition
  delta <- genRangeLength
  let x2 = x1 { _character = _character x1 + delta }
  pure $ Range x1 x2
  where
    genRangeLength :: RVar UInt
    genRangeLength = fromInteger <$> Fu.uniform 5 50

genPosition :: RVar Position
genPosition = Position
  <$> (fromInteger <$> Fu.uniform 0 10000)
  <*> (fromInteger <$> Fu.uniform 0 150)

filterRangeList :: Range -> [Range] -> [Range]
filterRangeList r = filter (isSubrangeOf r)

main :: IO ()
main = do
  rangeLists@[rangeList100, rangeList1000, rangeList10000]
    <- traverse (Fu.sampleFrom Random.globalStdGen . genRangeList) [100, 1000, 10000]
  [rangeMap100, rangeMap1000, rangeMap10000] <- evaluate $ force $ map (RangeMap.fromList id) rangeLists
  targetRange <- Fu.sampleFrom Random.globalStdGen genRange
  Criterion.Main.defaultMain
    [ Criterion.bgroup "List"
        [ Criterion.bench "Size 100" $ Criterion.nf (filterRangeList targetRange) rangeList100
        , Criterion.bench "Size 1000" $ Criterion.nf (filterRangeList targetRange) rangeList1000
        , Criterion.bench "Size 10000" $ Criterion.nf (filterRangeList targetRange) rangeList10000
        ]
    , Criterion.bgroup "RangeMap"
        [ Criterion.bench "Size 100" $ Criterion.nf (RangeMap.filterByRange targetRange) rangeMap100
        , Criterion.bench "Size 1000" $ Criterion.nf (RangeMap.filterByRange targetRange) rangeMap1000
        , Criterion.bench "Size 10000" $ Criterion.nf (RangeMap.filterByRange targetRange) rangeMap10000
        ]
    ]
