{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F
       deriving Eq

foo :: X -> Int
foo x = case x of
  y | y == A -> 1
