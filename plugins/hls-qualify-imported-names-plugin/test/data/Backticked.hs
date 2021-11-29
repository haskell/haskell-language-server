module Backticked where

import Prelude

f a b = a `elem` b

g a b =
  let h = f a b
  in a `elem` b

