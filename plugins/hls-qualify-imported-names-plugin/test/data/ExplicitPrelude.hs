module ExplicitPrelude where

import Prelude

f :: String -> Int -> Maybe Bool
f a b = Just False 

class Functor f => MyClass f where
  method :: f Int

