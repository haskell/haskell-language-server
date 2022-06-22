{-# LANGUAGE InstanceSigs #-}
module LocalClassDefine where

data A
class F a where
    f :: a -> Int

instance F A where
    f :: A -> Int
    f = _
