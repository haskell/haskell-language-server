module Test
( SomeData(..)
) where

-- no vertical bar comment
class Semigroup a => SomeData a

instance SomeData All
