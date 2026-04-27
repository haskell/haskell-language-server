{-# LANGUAGE Haskell2010 #-}

module QualifiedPositionalConstruction where

import qualified Data.Foldable as Foldable

data Foo = Foo { bar :: [Int] }

foo :: Foo
foo = Foo { bar = (Foldable.toList [1, 2, 3]) }