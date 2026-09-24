{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F
-- XXX It is crucial that this file ends without a line terminator!
foo :: X -> Int
foo x = case x of