module Pragma where

data F where
  G :: {-# UNPACK #-}Int -> F
  H :: {-# NOUNPACK #-}Char -> F
