module Test where
import Data.Monoid

-- | a comment
class Semigroup a => SomeData a

instance SomeData All
