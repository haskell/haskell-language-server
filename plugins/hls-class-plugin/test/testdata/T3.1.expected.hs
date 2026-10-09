module T3 where

class Test a where
  f :: a
  f = h
  g :: a
  h :: a
  h = f
  {-# MINIMAL f, g | g, h #-}

instance Test [a] where
  f = _
  g = _
