module Typeclass where

class Equality a where
    equals :: a -> a -> Bool

instance Equality Int where
  equals = (==)

allEqual :: Equality a => [a] -> Bool
allEqual = all =<< equals . head
