{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"
{-# LANGUAGE TupleSections #-}

data Something = Something {
    foo :: !String,
    bar :: !Int
}

tupleSection = (1, ) <$> Just 2
