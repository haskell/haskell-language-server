module Record where

data Foo where
  Foo :: {bar :: Char, baz :: Int} -> Foo
