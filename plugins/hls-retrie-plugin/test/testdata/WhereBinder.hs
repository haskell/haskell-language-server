module WhereBinder where

-- inline every use of a where binding from its binder
f x = e x + e 1
  where
    e y = y * 2
