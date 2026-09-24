{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

foo :: Int -> Int
foo x = do case x of
              1 -> 1
              2 -> 2
              i | i < 4 || i > 5 -> 3
