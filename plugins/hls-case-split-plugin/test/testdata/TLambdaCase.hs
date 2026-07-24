{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
module T1 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Int
foo x = (\case) x
