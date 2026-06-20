{-# LANGUAGE CPP #-}
module CppCtorIntra
  ( Foo(Foo1
#ifdef EXAMPLE_FLAG
      , Bar
#endif
      )
  ) where

data Foo = Foo1 | Bar | Foo2

foo :: Int
foo = 0
