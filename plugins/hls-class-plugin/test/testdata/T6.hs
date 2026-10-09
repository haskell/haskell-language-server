module T6 where

data X = X | Y

class Test a where
  f :: a -> a
  f = h

  g :: a

  h :: a -> a
  h = f

  i :: a

  {-# MINIMAL f, g, i | g, h #-}

instance Test X where
  f X = X
  f Y = Y
  i = undefined
