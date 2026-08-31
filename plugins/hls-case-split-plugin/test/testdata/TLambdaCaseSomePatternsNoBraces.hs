{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
{-# LANGUAGE LambdaCase #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Maybe Int
foo x = pure x >>= \case A -> Just 1
                         B -> Just 2
