{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

data A = A
data B = B
data X = X
data Y = Y


data Pairrow ax by  where
  Pairrow :: (a -> b) -> (x -> y) -> Pairrow '(a, x) '(b, y)

test2 :: (A -> B) -> (X -> Y) -> Pairrow '(A, X) '(B, Y)
test2 = _

