module SingleDeriving where

data Foo a b = Bar b a
 deriving (Eq)
