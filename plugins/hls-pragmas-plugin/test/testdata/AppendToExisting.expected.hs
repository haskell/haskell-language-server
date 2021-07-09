-- | Doc before pragma
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module AppendToExisting where

data Record = Record
  { a :: Int,
    b :: Double,
    c :: String
  }

f Record{a, b} = a
