{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}

module HsExpanded1 where
import Prelude

ifThenElse :: Int -> Int -> Int ->  Int
ifThenElse x y z = x + y + z

data MyRec = MyRec
  { foo :: Int }

myRecExample = MyRec 5

convertMe :: Int
convertMe =
  if (let MyRec {foo} = myRecExample
      in foo) then 1 else 2