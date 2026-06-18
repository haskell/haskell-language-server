module QualifiedName where

import Data.List.NonEmpty (NonEmpty(..))

foo :: NonEmpty a -> [a]
foo xs = NE.toList xs
