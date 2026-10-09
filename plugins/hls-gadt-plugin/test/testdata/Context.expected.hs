module Context where

data Foo a where
  Bar :: (Eq a, Show b, Show a) => a -> b -> Foo a
  Baz :: (Eq a, Show c) => c -> c -> Foo a
