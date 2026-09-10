module NestedLet where

-- inline a let binding
f x =
  let e x = x
  in e x
