{-# LANGUAGE DuplicateRecordFields #-}

module FVec (RecA(..), RecB(..)) where

data Vec a

newtype RecA a = RecA { fromList :: [a] -> Vec a }

newtype RecB a = RecB { fromList :: [a] -> Vec a }
