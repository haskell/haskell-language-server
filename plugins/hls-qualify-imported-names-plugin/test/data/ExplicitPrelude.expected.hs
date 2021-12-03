module ExplicitPrelude where

import Prelude

f :: Prelude.String -> Prelude.Int -> Prelude.Maybe Prelude.Bool
f a b = Prelude.Just Prelude.False 

class Prelude.Functor f => MyClass f where
  method :: f Prelude.Int

