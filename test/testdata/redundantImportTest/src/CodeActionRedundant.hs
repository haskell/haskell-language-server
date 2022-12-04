{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE PatternSynonyms #-}
module CodeActionRedundant where
import Data.List
-- We need a non-reduntant import in the import list
-- to properly test the removal of the singular redundant item
import Data.Sequence (pattern Empty, singleton)
main :: IO ()
main = putStrLn "hello"
  where unused = Data.Sequence.singleton 42
