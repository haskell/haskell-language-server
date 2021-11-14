module A (module B, a, b) where

import B

a :: Int -> Int
a = id

b :: String -> String
b = id

