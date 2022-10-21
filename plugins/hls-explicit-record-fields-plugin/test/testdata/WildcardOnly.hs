{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}

module WildcardOnly where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {..} = show foo ++ show bar ++ show baz
