module NestedWhere where

-- inline a where binding
f x = e x
  where
    e x = x
