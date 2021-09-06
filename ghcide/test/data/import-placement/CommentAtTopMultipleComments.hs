module Test
( SomeData(..)
) where

-- | Another comment
data SomethingElse = SomethingElse

-- | Some comment
class Semigroup a => SomeData a

instance SomeData All
