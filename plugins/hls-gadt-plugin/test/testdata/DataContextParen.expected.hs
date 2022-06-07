module DataContextParen where

data F a where
  G :: Eq a => a -> F a
