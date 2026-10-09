module Backticked where

import Prelude

f a b = a `Prelude.elem` b

g a b =
  let h = f a b
  in a `Prelude.elem` b

