{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
{-# LANGUAGE PatternSynonyms #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

pattern Foo :: X
pattern Foo = E
{-# COMPLETE D, Foo, C #-}

foo :: X -> Int
foo x = case x of
