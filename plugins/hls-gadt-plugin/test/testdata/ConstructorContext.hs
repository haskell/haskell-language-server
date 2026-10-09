module ConstructorContext where

data Foo = forall a. (Show a) => Bar a
