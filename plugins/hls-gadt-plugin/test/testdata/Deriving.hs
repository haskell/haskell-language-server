module Deriving where

data Foo = Bar Int | Baz Char String deriving (Show, Eq)
