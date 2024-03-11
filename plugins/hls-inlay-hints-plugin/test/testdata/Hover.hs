{-# LANGUAGE RecordWildCards #-}
module Hover where
import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Function       (on)
f1 = (++)
f2 = ($)
f3 = (.)
f4 = (+)
f5 = 1 - 2
f6 = (<>)
f7 = (>>=)
f8 = (>=>)
f9 = elem
f10 = on
f11 = (||)
f12 = mod
f13 = (**)
f14 = (^)
f15 = (<$)
f16 = seq
f17 = (<|>)

infixr 7 >>:
infix 9 >>::
data F = G
    { (>>:)  :: Int -> Int -> Int
    , c      :: Int
    , (>>::) :: Char
    }
f G{..} = undefined

infixl 1 `f`

infixr 9 >>>:
(>>>:) :: Int -> Int
(>>>:) x = 3

infixl 3 ~\:
(~\:) x y = 3
