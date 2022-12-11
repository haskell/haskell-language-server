module Operator where

f :: a
f = undefined
    where
        g :: (a -> b) -> a -> b
        g = ($)
