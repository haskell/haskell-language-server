module ConstructorDef (T (..), e) where

data T = MkT Int

e :: Int -> T
e x = MkT x
