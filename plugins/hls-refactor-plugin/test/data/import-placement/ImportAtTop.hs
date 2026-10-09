module Test
( SomeData(..)
) where

import Data.Char

{- Some multi 
   line comment 
-}
class Semigroup a => SomeData a

-- | a comment
instance SomeData All
