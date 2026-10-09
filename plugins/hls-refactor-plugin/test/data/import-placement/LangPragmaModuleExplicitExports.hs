{-# LANGUAGE OverloadedStrings #-}

module Test
( SomeData(..)
) where

class Semigroup a => SomeData a

instance SomeData All
