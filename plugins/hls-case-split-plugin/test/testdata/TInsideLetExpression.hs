{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T8 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> a
foo x = let  r = x
   in       case r of
