module Test
( SomeData(..)
) where
import Data.Monoid

data Something = Something

-- | some comment
class Semigroup a => SomeData a

instance SomeData All
