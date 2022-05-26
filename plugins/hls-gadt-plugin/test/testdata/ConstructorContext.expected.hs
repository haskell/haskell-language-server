module ConstructorContext where

data Foo where
  Bar :: Show a => a -> Foo
