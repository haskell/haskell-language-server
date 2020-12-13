{-# OPTIONS_GHC -Wunused-imports #-}
module MultipleImports where
import Data.Foldable
import Data.Maybe
foo :: Int
foo = fromJust (Just 3)
