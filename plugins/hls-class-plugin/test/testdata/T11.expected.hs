{-# LANGUAGE InstanceSigs #-}
module T11 where

data A
class F a where
    f :: a -> Int

instance F A where
    f :: A -> Int
    f = _
