{-# LANGUAGE Haskell2010 #-}

module PolymorphicRecordConstruction where

data MyRec m = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: () -> MyRec ()
convertMe _ =
  let a = 3
      b = 5
      c = 'a'
  in MyRec a b c
