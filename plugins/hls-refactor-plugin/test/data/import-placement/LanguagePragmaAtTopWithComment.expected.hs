{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid

-- | comment
class Semigroup a => SomeData a

instance SomeData All
