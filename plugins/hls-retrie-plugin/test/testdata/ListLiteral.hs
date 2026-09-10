module ListLiteral where

e :: Int -> Int
e x = x * 2

f :: [Int]
f = [e 1, e 2, e 3]
