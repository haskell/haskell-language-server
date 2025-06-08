{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HsExpanded1DuplicateRecordFields where
import Prelude

ifThenElse :: Int -> Int -> Int ->  Int
ifThenElse x y z = x + y + z

data MyRec = MyRec
  { foo :: Int }

myRecExample = MyRec 5

convertMe :: Int
convertMe =
  if (let MyRec {..} = myRecExample
      in foo) then 1 else 2
