module Test
( SomeData(..)
) where

-- | Some comment
class Semigroup a => SomeData a

instance SomeData All
