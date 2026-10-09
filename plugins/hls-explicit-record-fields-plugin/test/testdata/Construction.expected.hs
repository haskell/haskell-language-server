{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Construction where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: () -> MyRec
convertMe _ =
  let foo = 3
      bar = 5
      baz = 'a'
  in MyRec {foo, bar, baz}
