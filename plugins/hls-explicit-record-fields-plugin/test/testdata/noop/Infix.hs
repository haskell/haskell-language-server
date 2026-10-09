{-# LANGUAGE Haskell2010 #-}

module Infix where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  }

convertMe :: MyRec -> String
convertMe (foo' `MyRec` bar') = show foo' ++ show bar'
