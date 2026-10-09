module Test
( SomeData(..)
) where

import Data.Char
import Data.Monoid

{- Some multi 
   line comment 
-}
class Semigroup a => SomeData a

-- | a comment
instance SomeData All
