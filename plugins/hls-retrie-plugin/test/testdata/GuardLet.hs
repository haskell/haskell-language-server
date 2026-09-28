module GuardLet where

f :: Int -> Int
f y
  | let e x = x + 1
  , y > 0 = e y
  | otherwise = 0
