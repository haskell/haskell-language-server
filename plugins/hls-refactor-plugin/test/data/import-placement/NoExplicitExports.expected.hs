module Test where
import Data.Monoid

newtype Something = S { foo :: Int }

class Semigroup a => SomeData a

instance SomeData All
