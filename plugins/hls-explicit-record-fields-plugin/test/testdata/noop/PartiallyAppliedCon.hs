{-# LANGUAGE Haskell2010 #-}

module PartiallyAppliedCon where

data T = MkT { fa :: Int, fb :: Char }

foo :: Int -> Char -> T
foo x = MkT x
