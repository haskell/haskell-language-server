module Test
( SomeData(..)
) where
import Data.Monoid

{- Some multi 
   line comment 
-}
class Semigroup a => SomeData a

instance SomeData All
