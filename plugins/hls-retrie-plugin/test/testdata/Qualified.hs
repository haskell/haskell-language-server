module Qualified where

e :: Int -> Int
e x = x - x

f y = Qualified.e y
