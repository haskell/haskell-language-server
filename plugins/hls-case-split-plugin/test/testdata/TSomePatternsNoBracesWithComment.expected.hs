{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Int
foo x = case x of -- comment 1
  A -> 1 -- comment 2
  B -> 2 -- comment 3
  C _ -> _
  D _ _ -> _
  E -> _
  F -> _
