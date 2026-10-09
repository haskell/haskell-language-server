module Forall where

data Foo = forall a b. (Show a) => Bar a b a
