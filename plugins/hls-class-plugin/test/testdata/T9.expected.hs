{-# LANGUAGE InstanceSigs #-}
module T9 where

data A aaa
instance Applicative A where
  pure :: a -> A a
  pure = _
  (<*>) :: A (a -> b) -> A a -> A b
  (<*>) = _
