{-# LANGUAGE CPP #-}
module CppExportOtherBranch
  (
#ifndef EXAMPLE_FLAG
    foo
#endif
  ) where

foo :: Int
foo = 1

bar :: Int
bar = 2
