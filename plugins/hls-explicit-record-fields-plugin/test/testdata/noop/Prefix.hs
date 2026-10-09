{-# LANGUAGE Haskell2010 #-}

module Prefix where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  }

convertMe :: MyRec -> String
convertMe (foo' `MyRec` bar') = show foo' ++ show bar'
