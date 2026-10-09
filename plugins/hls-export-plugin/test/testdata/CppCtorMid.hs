{-# LANGUAGE CPP #-}
module CppCtorMid
  ( Foo(Foo1)
  , bar
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

data Foo = Foo1 | Foo2

bar :: Int
bar = 0

flagged :: Int
flagged = 0
