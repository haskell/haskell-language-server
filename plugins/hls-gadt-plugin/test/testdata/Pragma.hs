module Pragma where

data F = G{-# UNPACK #-}Int
    | H {-# NOUNPACK #-} Char
