{-# LANGUAGE RankNTypes #-}

module HigherRankFunction where

f :: (forall a. [a] -> Int) -> [b] -> [c] -> (Int, Int)
f l xs ys = (l xs, l ys)
