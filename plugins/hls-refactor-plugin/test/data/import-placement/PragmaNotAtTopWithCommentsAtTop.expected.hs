{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}
-- another comment

{-# LANGUAGE TupleSections #-}
import Data.Monoid
{- some comment -}


class Semigroup a => SomeData a
instance SomeData All

#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

addOne :: Int -> Int
addOne x = x + 1
