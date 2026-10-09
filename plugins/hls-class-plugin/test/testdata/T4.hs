module T4 where

class Test a where
  _f :: a
  {-# MINIMAL _f #-}

instance Test Int where
