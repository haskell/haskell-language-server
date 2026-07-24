{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T7 where

data X = A
       | B
       | C { foo :: Int }
       | D { bar :: Int, baz :: Int }
       | E
       | F

f :: X -> Int
f x = case x of
