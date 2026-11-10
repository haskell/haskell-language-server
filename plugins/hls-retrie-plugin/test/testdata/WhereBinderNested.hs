module WhereBinderNested where

-- inline a use nested in the argument of another
f x = e (e x)
  where
    e y = 2 * y
