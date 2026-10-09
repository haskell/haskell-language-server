{-# LANGUAGE InstanceSigs #-}
module InsertWithoutPragma where

data A
instance Eq A where
  (==) :: A -> A -> Bool
  (==) = _
