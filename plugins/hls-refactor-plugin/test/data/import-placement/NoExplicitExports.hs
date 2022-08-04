module Test where

newtype Something = S { foo :: Int }

class Semigroup a => SomeData a

instance SomeData All
