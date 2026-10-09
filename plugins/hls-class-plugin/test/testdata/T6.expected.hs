{-# LANGUAGE GHC2021#-}
module T6 where

data A
instance Eq A where
  (==) :: A -> A -> Bool
  (==) = _
