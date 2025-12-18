module Typeclass where

f :: a
f = undefined
    where
        g :: Num a => a -> a -> a
        g a b = a + b
