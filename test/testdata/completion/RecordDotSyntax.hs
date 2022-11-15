{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test where

import qualified Data.Maybe as M

data MyRecord = MyRecord1
  { a :: String
  , b :: Integer
  , c :: MyChild
  }
  | MyRecord2 { a2 :: String
  , b2 :: Integer
  , c2 :: MyChild
  } deriving (Eq, Show)

newtype MyChild = MyChild
  { z :: String
  } deriving (Eq, Show)

x = MyRecord1 { a = "Hello", b = 12, c = MyChild { z = "there" } }

y = x.a ++ show x.b 

y2 = x.c.z 

