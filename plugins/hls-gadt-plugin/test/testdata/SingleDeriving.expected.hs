module SingleDeriving where

data Foo a b where
  Bar :: b -> a -> Foo a b
  deriving (Eq)
