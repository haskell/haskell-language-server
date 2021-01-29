module DVec (Vec, (++), cons, fromList, snoc) where

import Prelude hiding ((++))

data Vec a

(++) :: Vec a -> Vec a -> Vec a
(++) = undefined

fromList :: [a] -> Vec a
fromList = undefined

cons :: a -> Vec a -> Vec a
cons = undefined

snoc :: Vec a -> a -> Vec a
snoc = undefined
