{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language OverloadedRecordDot #-}
module HsExpanded1 where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

convertMe :: () -> Int
convertMe _ =
  let foo = 3
      bar = 5
      baz = 'a'
  in MyRec {..}.foo