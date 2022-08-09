{-# LANGUAGE InstanceSigs #-}
module Inline where

data A
instance Eq A where (==) :: A -> A -> Bool
                    (==) = _
