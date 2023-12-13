{-# LANGUAGE TypeFamilies #-}
module TypeFamily where

type family Foo a where
  Foo Int = Int
  Foo a = String
