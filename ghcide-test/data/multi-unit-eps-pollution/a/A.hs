module A (AType, MyClass(..)) where

data AType = AType Int

class MyClass a where
  myMethod :: a -> String

instance MyClass AType where
  myMethod (AType n) = "AType " ++ show n
