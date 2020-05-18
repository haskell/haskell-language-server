{-# LANGUAGE PatternSynonyms #-}
module Symbols where

import Data.Maybe

foo = bar
  where bar = 42 + dog
          where (dog, cat) = (1234, "meow")

data MyData = A Int
            | B String

pattern TestPattern :: Int -> MyData
pattern TestPattern x = A x
