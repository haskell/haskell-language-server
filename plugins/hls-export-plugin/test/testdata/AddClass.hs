module AddClass (Foo (..), Bar) where

class Foo a where
  foo1 :: a -> Int

class Bar a where
  bar1 :: a -> Int

class Baz a where
  baz1 :: a -> Int
