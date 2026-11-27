module Partial where

-- inline a function that's not fully applied
e :: Int -> Int
e x = x * 2

f :: [Int]
f = map e [1, 2, 3]
