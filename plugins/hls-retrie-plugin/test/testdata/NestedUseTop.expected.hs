module NestedUseTop where

e :: Int -> Int
e y = 2 * y

-- inline the inner of two nested uses
f :: Int -> Int
f x = e (2 * x)
