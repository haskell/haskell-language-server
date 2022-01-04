{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall,
   OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- another comment
-- oh
{- multi line 
comment
-}

{-# LANGUAGE TupleSections #-}
import Data.Monoid
{- some comment -}

-- again
class Semigroup a => SomeData a
instance SomeData All

#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

addOne :: Int -> Int
addOne x = x + 1
