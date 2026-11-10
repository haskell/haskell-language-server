module ChildExportDef (e, method) where

class C a where
  method :: a -> Int

instance C Int where
  method x = x + 1

-- method is exported on its own, so an import must not name its class
e :: Int -> Int
e x = method (x :: Int)
