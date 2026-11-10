module WhereBinderNested where

-- inline a use nested in the argument of another
f x = 2 * (2 * x)
  where
    e y = 2 * y
