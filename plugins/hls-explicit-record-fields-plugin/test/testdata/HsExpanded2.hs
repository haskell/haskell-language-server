{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module HsExpanded2 where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , baz :: Char
  }

data YourRec = YourRec
  { foo2 :: MyRec
  , bar2 :: Int
  , baz2 :: Char
  }

myRecExample = MyRec {..}
  where
    foo = 5
    bar = 6
    baz = 'a'

yourRecExample = YourRec {..}
    where
        foo2 = myRecExample
        bar2 = 5
        baz2 = 'a'

convertMe :: () -> Int
convertMe _ =
  (let MyRec{..} = myRecExample
       YourRec{..} = yourRecExample
    in foo2).foo