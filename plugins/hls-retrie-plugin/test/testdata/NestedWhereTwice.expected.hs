module NestedWhereTwice where

-- inline one of two uses of a where binding
f x = e x + 1 * 2
  where
    e y = y * 2
