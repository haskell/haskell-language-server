module TypeVariable where

data Foo a f where
  Foo :: a -> Foo a f
  Bar :: (f a) -> Foo a f
