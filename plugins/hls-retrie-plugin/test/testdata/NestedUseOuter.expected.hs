module NestedUseOuter where

-- inline the outer of two nested uses
f x = 2 * e x
  where
    e y = 2 * y
