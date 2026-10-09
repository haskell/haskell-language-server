module Infix where

data Foo where
  (:->) :: Int -> Char -> Foo
  deriving ()
