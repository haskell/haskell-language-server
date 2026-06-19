{-# LANGUAGE CPP #-}
module CppCtorUpgrade
  ( Bar
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

data Bar = Bar1 | Bar2

flagged :: Int
flagged = 0
