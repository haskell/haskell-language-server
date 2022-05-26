module TypeVariable where

data Foo a f = Foo a | Bar (f a)
