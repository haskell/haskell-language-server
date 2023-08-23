{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}

module HsExpanded2 where
import Prelude

ifThenElse :: Int -> Int -> Int ->  Int
ifThenElse x y z = x + y + z

data MyRec = MyRec
  { foo :: Int }

data YourRec = YourRec
  { bar :: Int }

myRecExample = MyRec 5

yourRecExample = YourRec 3

convertMe :: Int
convertMe =
  if (let MyRec {..} = myRecExample
          YourRec {..} = yourRecExample
      in bar) then 1 else 2