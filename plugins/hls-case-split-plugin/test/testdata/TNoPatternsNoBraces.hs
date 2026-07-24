{-# OPTIONS_GHC -ddump-parsed-ast    #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T1 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Int
foo x = case x of
