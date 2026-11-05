module CrossModuleNotExported (e) where

k :: Int
k = 1

e :: Int -> Int
e x = x + k
