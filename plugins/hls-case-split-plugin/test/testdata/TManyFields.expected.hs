{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B Int
       | C Int Int
       | D Int Int Int
       | E Int Int Int Int
       | F Int Int Int Int Int

foo :: X -> Int
foo x = case x of
          A {} -> 1
          B _ -> 2
          C _ _ -> _
          D _ _ _ -> _
          E {} -> _
          F {} -> _
