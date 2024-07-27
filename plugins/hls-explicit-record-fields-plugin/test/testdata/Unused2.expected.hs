{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Unused2 where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {foo, bar} = let baz = "baz" in show foo ++ show bar ++ baz
