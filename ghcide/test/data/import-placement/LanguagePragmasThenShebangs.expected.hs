{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"
import Data.Monoid

-- some comment
class Semigroup a => SomeData a

instance SomeData All
