{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F
       | G
       | H

foo :: X -> Int
foo x = case x of
        A -> 3
        B -> 4; C _ -> 5
        D _ _ -> _; E -> _
        F -> _; G -> _
        H -> _
