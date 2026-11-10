module NestedUse where

-- inline the inner of two nested uses
f x = e (e x)
  where
    e y = 2 * y
