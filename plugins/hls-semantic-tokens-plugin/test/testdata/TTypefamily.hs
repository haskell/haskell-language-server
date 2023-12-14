{-# LANGUAGE TypeFamilies #-}
module TTypefamily where

type family Foo a where
  Foo Int = Int
  Foo a = String
