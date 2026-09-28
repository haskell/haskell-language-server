module ShadowLocal where

e :: Int -> Int
e x = x * 2

f :: Int
f = e (let e x = x + 1 in e 3)
