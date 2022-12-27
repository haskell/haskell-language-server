module AllMethodsRequired where

class Test a where
  f :: a
  g :: a
  {-# MINIMAL f,g #-}

instance Test [a] where

