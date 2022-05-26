module Deriving where

data Foo where
  Bar :: Int -> Foo
  Baz :: Char -> String -> Foo
  deriving (Show, Eq)
