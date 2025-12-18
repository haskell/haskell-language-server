module Qualified where

import qualified Data.Map as Map

f :: a
f = undefined
    where
         g :: Map.Map Bool Char
         g = Map.singleton True 'c'
