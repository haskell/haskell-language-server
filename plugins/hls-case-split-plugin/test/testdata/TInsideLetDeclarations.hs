{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T10 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> a
foo x = let r = case x of
      in r
