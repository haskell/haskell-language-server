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
              A -> _
              B -> _
              C _ -> _
              D _ _ -> _
              E -> _
              F -> _
      in r
