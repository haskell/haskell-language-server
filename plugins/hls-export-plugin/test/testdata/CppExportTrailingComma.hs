{-# LANGUAGE CPP #-}
module CppExportTrailingComma
  ( foo,
#ifdef EXAMPLE_FLAG
    flagged,
#endif
  ) where

foo :: Int
foo = 1

flagged :: Int
flagged = 2

baz :: Int
baz = 3
