{-# LANGUAGE InstanceSigs #-}
module T8 where

data A
instance Eq A where
  (==) :: A -> A -> Bool
  (==) = _
