module SingleDerivingGHC92 where

data Foo a b where
  Bar :: b -> a -> Foo a b
  deriving (Eq)
