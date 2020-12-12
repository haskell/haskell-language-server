module Context where
import Control.Concurrent as Conc
foo :: Int -> Int
foo x = abs $ id 42
