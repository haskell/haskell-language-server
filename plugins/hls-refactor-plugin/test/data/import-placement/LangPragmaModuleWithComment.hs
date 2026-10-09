{-# LANGUAGE OverloadedStrings #-}

module Test
( SomeData(..)
) where

-- comment
class Semigroup a => SomeData a

instance SomeData All
