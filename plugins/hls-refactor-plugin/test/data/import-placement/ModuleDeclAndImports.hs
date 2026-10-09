module Test
( SomeData(..)
) where
import Data.Char
import Data.Array

class Semigroup a => SomeData a

instance SomeData All
