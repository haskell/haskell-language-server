module T2 where

data X a
  = A a
  | B

instance
  (Eq a) => Eq (X a)
 where

instance
  Functor X where
  fmap = _
