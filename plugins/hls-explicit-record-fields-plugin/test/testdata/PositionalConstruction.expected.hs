{-# LANGUAGE Haskell2010 #-}

module PositionalConstruction where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: () -> MyRec
convertMe _ =
  let a = 3
      b = 5
      c = 'a'
  in MyRec { foo = a, bar = b, baz = c }
