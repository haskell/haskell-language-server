{-# LANGUAGE CPP #-}
module CppExportElse
  ( foo
#ifdef EXAMPLE_FLAG
  , windows
#else
  , posix
#endif
  ) where

foo :: Int
foo = 1

windows :: Int
windows = 1

posix :: Int
posix = 2

extra :: Int
extra = 3
