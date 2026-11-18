module Field where

data R = R { fld :: Int }

g :: Int -> Int
g x = x

f :: R -> Int
f r = g (fld r)
