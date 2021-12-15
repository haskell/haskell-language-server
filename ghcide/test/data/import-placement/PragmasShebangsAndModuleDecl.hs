{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"

module Test where

class Semigroup a => SomeData a

instance SomeData All
