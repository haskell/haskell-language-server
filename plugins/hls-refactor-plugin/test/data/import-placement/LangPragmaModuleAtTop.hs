{-# LANGUAGE OverloadedStrings #-}

module Test where

class Semigroup a => SomeData a

instance SomeData All
