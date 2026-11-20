module Ops ((<+>)) where

infixr 5 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + b
