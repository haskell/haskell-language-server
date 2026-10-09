#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"

module AfterShebang where

data Record = Record
  { a :: Int,
    b :: Double,
    c :: String
  }

f Record{a, b} = a
