{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F
{-# COMPLETE D, B, C #-}

foo :: X -> Int
foo x = case x of
  D _ _ -> _
  B -> _
  C _ -> _
