{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
module DefaultImplementation where

class Test1 a where

class Test a where
  f :: a -> a
  default f :: Test1 a => a -> a
  f = id

data X = X | Y

instance Test X where
  f :: X -> X
  f = _
