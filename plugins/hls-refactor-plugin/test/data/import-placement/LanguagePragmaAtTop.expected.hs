{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid

class Semigroup a => SomeData a

instance SomeData All
