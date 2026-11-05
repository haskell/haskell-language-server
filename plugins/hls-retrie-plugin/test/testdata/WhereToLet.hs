module WhereToLet where

e x = a x
  where
    a x = x + 1

-- where bindings in the expression are converted to let bindings
f x = e x
