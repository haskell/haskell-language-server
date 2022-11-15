{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}

module WithExplicitBind where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {foo = foo', ..} = show foo' ++ show bar ++ show baz
