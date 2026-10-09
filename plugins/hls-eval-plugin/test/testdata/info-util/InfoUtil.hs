module InfoUtil
  ( Eq
  , Ord
  , Foo (..)
  , Bar (..)
  , Baz
  )
where

import           Prelude (Eq, Ord)

data Foo = Foo1 | Foo2
  deriving (Eq, Ord)

data Bar = Bar1 | Bar2 | Bar3
  deriving (Eq, Ord)

class Baz t
instance Baz Foo
instance Baz Bar
