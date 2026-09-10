module WhereToLetSubst where

e x = y
  where
    y = x + 1

f = let
      y = 1 + 1
     in y
