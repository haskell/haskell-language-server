{-# LANGUAGE Haskell2010 #-}

module ExplicitBinds where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {foo = foo', bar = bar', baz = baz'} = show foo' ++ show bar' ++ show baz'
