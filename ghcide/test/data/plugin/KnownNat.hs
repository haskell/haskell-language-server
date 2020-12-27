{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}
module KnownNat where
import Data.Proxy
import GHC.TypeLits

f :: forall n. KnownNat n => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy (n+2))
foo :: Int -> Int -> Int
foo a _b = a + c
