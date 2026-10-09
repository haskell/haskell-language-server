{-# LANGUAGE CPP #-}
module CppCtorExtend
  ( Foo(Foo1)
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

data Foo = Foo1 | Foo2

flagged :: Int
flagged = 0
