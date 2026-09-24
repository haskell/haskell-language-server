{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
{-# LANGUAGE OrPatterns #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Int
foo x = case x of
          A -> 3
          a@(B; C _) -> case a of
                          B -> 3
                          C _ -> _
