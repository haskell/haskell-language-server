module Test
( SomeData(..)
) where

data Something = Something

-- | some comment
class Semigroup a => SomeData a

instance SomeData All
