{-# LANGUAGE DuplicateRecordFields #-}
module DupRecFields where

newtype One = One { accessor :: Int }
newtype Two = Two { accessor :: Int }
