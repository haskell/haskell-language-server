{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T8 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> a
foo x = bar x
  where
    bar x' = case x' of
