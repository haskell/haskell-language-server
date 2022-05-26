module Forall where

data Foo where
  Bar :: Show a => a -> b -> a -> Foo
