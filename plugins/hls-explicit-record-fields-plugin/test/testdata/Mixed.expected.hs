{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mixed where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  , quux :: Double
  }

convertMe :: MyRec -> String
convertMe MyRec {foo, bar = bar', baz} = show foo ++ show bar' ++ show baz
