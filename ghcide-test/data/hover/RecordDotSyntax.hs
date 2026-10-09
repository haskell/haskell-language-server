{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors #-}

module RecordDotSyntax ( module RecordDotSyntax) where

import qualified Data.Maybe as M

data MyRecord = MyRecord
  { a :: String
  , b :: Integer
  , c :: MyChild
  } deriving (Eq, Show)

newtype MyChild = MyChild
  { z :: String
  } deriving (Eq, Show)

x = MyRecord { a = "Hello", b = 12, c = MyChild { z = "there" } }
y = x.a ++ show x.b ++ x.c.z
