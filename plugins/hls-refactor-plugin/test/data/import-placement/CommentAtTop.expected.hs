module Test
( SomeData(..)
) where
import Data.Monoid

-- | Some comment
class Semigroup a => SomeData a

instance SomeData All
