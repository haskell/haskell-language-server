{-# LANGUAGE CPP #-}
module CppCtorAppend
  ( foo
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

foo :: Int
foo = 1

data Baz = Baz1 | Baz2

flagged :: Int
flagged = 0
