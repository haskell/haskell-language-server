module Constraint where

data T a b where
  F :: (Ord a) => a -> T a b
  G :: (Ord a) => b -> T a b
