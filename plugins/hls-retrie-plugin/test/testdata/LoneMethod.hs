-- the method is exported on its own, without its class
module LoneMethod (method2) where

class C2 a where
  method2 :: a -> Int

instance C2 Int where
  method2 x = x * 2
