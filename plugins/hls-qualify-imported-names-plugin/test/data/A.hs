module A (module B, a, b, op) where

import B

a :: Int -> Int
a = id

b :: String -> String
b = id

op :: Int -> Int -> Int
op = (+)
