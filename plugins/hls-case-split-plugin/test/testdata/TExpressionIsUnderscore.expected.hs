{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T1 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: Int
foo = case      _     :: X of
  A -> _
  B -> _
  C _ -> _
  D _ _ -> _
  E -> _
  F -> _
