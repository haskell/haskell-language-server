{-# LANGUAGE InstanceSigs #-}
module T7 where

data A
instance Eq A where
  (==) :: A -> A -> Bool
  (==) = _
