{-# LANGUAGE OverloadedStrings #-}

module Test
( SomeData(..)
) where
import Data.Monoid

class Semigroup a => SomeData a

instance SomeData All
