{-# LANGUAGE CPP #-}
module CppExportInclude
  ( foo
#include "CppExportInclude.h"
  ) where

foo :: Int
foo = 1

included :: Int
included = 2

extra :: Int
extra = 3
