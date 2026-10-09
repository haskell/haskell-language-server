{-# LANGUAGE CPP #-}
module CppExportHead
  (
#ifndef EXAMPLE_FLAG
    foo
#endif
  ) where

foo :: Int
foo = 1

bar :: Int
bar = 2
