module WhereBinder where

-- inline every use of a where binding from its binder
f x = x * 2 + 1 * 2
  where
    e y = y * 2
