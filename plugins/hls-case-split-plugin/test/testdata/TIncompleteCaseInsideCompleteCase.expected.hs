{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
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
          a -> case a of
            B -> 4
            C _ -> 5
            D _ _ -> _
            E -> _
            F -> _
