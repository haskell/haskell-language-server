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
    bar :: X -> a
    bar = baz
      where
        baz :: X -> a
        baz x' = case x' of
