module Test
( SomeData(..)
) where

newtype Something = S { foo :: Int }

-- | a comment
class Semigroup a => SomeData a

instance SomeData All
