{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OrPatterns #-}
module T1 where

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
                 C _ -> 4
          D _ _ -> _
          E -> _
          F -> _
