module MultiClauseTuple where

e :: Int -> Int -> Int
e 0 y = y
e x y = x * y

a :: Int -> Int
a k = case (k, 2) of
  (0, y) -> y
  (x, y) -> x * y
