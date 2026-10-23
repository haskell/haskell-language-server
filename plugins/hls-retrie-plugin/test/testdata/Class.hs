module Class where

class Identity x where
  e :: x -> x
  e x = x

f x = e x
