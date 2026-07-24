{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T2 where

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
