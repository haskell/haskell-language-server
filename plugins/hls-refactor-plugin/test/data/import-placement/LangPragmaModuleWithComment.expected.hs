{-# LANGUAGE OverloadedStrings #-}

module Test
( SomeData(..)
) where
import Data.Monoid

-- comment
class Semigroup a => SomeData a

instance SomeData All
