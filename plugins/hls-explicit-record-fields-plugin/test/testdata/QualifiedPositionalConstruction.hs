{-# LANGUAGE Haskell2010 #-}

module QualifiedPositionalConstruction where

import qualified Data.Foldable as Foldable

data Foo = Foo { bar :: [Int] }

foo :: Foo
foo = Foo (Foldable.toList [1, 2, 3])
