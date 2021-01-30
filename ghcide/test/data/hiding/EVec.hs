{-# LANGUAGE TypeOperators #-}
module EVec (Vec, (++), type (@@@), cons, fromList, snoc) where

import Prelude hiding ((++))

data Vec a

(++) :: Vec a -> Vec a -> Vec a
(++) = undefined

data (@@@) a b

fromList :: [a] -> Vec a
fromList = undefined

cons :: a -> Vec a -> Vec a
cons = undefined

snoc :: Vec a -> a -> Vec a
snoc = undefined
