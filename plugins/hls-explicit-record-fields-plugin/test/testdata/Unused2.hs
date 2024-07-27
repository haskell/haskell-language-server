{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}

module Unused2 where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {..} = let baz = "baz" in show foo ++ show bar ++ baz
