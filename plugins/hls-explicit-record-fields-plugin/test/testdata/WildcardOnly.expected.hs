{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module WildcardOnly where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {foo, bar, baz} = show foo ++ show bar ++ show baz
