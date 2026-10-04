module NestedUseOuter where

-- inline the outer of two nested uses
f x = e (e x)
  where
    e y = 2 * y
