module Test
( SomeData(..)
) where

import Data.Char
import Data.Bool
import Data.Eq

-- | A comment
class Semigroup a => SomeData a

-- | another comment
instance SomeData All
