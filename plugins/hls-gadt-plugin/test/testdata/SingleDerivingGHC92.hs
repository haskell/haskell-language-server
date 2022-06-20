module SingleDerivingGHC92 where

data Foo a b = Bar b a
 deriving (Eq)
