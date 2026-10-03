module NestedWhereTwice where

-- inline one of two uses of a where binding
f x = e x + e 1
  where
    e y = y * 2
