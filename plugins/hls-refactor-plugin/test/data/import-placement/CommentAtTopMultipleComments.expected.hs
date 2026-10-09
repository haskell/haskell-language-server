module Test
( SomeData(..)
) where
import Data.Monoid

-- | Another comment
data SomethingElse = SomethingElse

-- | Some comment
class Semigroup a => SomeData a

instance SomeData All
