module T9 where

class A a where
  a :: a

instance A Int where
  a = 1

class (A a) => B a where
  {-# MINIMAL b1 #-}
  b1 :: a
  b2 :: a
  b2 = b1

instance B Int where
  b1 = _
  b2 = _
