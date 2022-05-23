{-# LANGUAGE InstanceSigs #-}
module T12 where

data A
instance Eq A where
  (==) :: A -> A -> Bool
  (==) = _
