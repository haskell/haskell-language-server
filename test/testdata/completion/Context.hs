module Context where
import Control.Concurrent as Conc
foo :: Int -> Int -> Conc.MVar
foo x = abs 42
