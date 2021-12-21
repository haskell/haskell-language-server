module Test
( SomeData(..)
) where

import Data.Char
import Data.Bool
import Data.Eq
import Data.Monoid

-- | A comment
class Semigroup a => SomeData a

-- | another comment
instance SomeData All
