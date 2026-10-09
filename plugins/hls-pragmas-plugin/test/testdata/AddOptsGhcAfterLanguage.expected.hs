#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
-- | Doc Comment
{- Block -}

module BeforeDocComment where

test :: Int -> Integer
test x = x * 2
