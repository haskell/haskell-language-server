{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C { foo :: Int }
       | D { bar :: Int, baz :: Int }
       | E
       | F

f :: X -> Int
f x = case x of
