{-# LANGUAGE NamedFieldPuns #-}
module NamedFieldPuns where

data Record = Record
  { a :: Int,
    b :: Double,
    c :: String
  }

f Record{a, b} = a
