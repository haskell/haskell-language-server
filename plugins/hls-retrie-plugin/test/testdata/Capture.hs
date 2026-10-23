module Capture where

-- rename the let binding here to avoid capture
e :: Int -> Int
e x = let y = 1 in y + x

f :: Int -> Int
f y = e y
