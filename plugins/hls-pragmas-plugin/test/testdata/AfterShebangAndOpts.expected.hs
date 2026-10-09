#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TupleSections #-}

data Something = Something {
    foo :: !String,
    bar :: !Int
}

tupleSection = (1, ) <$> Just 2
