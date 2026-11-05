module ListLiteral where

e :: Int -> Int
e x = x * 2

f :: [Int]
f = [1 * 2, e 2, e 3]
