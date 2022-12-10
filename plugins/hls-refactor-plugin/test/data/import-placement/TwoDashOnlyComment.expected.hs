module Test
( SomeData(..)
) where
import Data.Monoid

-- no vertical bar comment
class Semigroup a => SomeData a

instance SomeData All
