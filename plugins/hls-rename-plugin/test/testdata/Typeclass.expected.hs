module Typeclass where

class Equal a where
    equals :: a -> a -> Bool

instance Equal Int where
  equals = (==)

allEqual :: Equal a => [a] -> Bool
allEqual = all =<< equals . head
