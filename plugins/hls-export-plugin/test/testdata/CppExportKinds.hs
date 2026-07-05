{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module CppExportKinds
  ( foo
#ifdef EXAMPLE_FLAG
  , flagged
#endif
  ) where

foo :: Int
foo = 1

(<|) :: Int -> Int -> Int
a <| b = a + b

pattern Zero :: Int
pattern Zero = 0

flagged :: Int
flagged = 2
