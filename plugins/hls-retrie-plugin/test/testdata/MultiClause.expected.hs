module MultiClause where

e :: Int -> Int
e 0 = 0
e n = n

f :: Int
f = case 5 of
  0 -> 0
  n -> n
