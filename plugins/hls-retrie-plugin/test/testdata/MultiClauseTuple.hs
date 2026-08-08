module MultiClauseTuple where

e :: Int -> Int -> Int
e 0 y = y
e x y = x * y

a :: Int -> Int
a k = e k 2
