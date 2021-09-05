{-# LANGUAGE OverloadedStrings #-}

module Test where
import Data.Monoid

class Semigroup a => SomeData a

instance SomeData All
