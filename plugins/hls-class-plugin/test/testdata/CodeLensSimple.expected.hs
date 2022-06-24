{-# LANGUAGE InstanceSigs #-}
module CodeLensSimple where

data A
instance Eq A where
    (==) :: A -> A -> Bool
    (==) = _

data B
instance Eq B where
    (==)= _
