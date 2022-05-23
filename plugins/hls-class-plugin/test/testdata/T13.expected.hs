{-# LANGUAGE InstanceSigs #-}
module T13 where

data A
instance Eq A where
    (==) :: A -> A -> Bool
    (==) = _
