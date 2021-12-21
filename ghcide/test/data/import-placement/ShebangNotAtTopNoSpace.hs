class Semigroup a => SomeData a
instance SomeData All

#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])"

f :: Int -> Int 
f x = x * x
