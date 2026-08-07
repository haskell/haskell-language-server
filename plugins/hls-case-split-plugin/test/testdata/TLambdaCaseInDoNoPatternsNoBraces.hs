{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
{-# LANGUAGE LambdaCase #-}
module T1 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Int
foo x = do  (  \   case) x
