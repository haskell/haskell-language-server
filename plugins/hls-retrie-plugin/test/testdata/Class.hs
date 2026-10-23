module Class where

class Identity x where
  e :: x -> x
  e x = x

instance Identity Int where
  e x = 5

-- the e refers to the Int instance, to be safe we don't offer any
-- action here
f :: Int -> Int
f x = e x
