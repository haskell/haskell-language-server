module Test
( SomeData(..)
) where
import Data.Char
import Data.Array
import Data.Monoid

class Semigroup a => SomeData a

instance SomeData All
