module CabalFileTest () where

-- With `StrictData` enabled in the `.cabal` file, Stan shouldn't complain here:
data A = A Int Int
