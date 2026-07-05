{-# LANGUAGE CPP #-}
module CppExportParenShared
  ( foo
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  , bar ) where

foo :: Int
foo = 1

flagged :: Int
flagged = 2

bar :: Int
bar = 3

baz :: Int
baz = 4
