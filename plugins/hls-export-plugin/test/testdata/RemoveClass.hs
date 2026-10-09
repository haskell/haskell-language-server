module RemoveClass (Foo (..), Bar, Baz (baz1)) where

class Foo a where
  foo1 :: a -> Int

class Bar a where
  bar1 :: a -> Int

class Baz a where
  baz1 :: a -> Int
  baz2 :: a -> Int

class Qux a where
  qux1 :: a -> Int
