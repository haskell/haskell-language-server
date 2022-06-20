{-# LANGUAGE ExplicitForAll #-}
module ScopedTypeVariables where

f :: forall a b. a -> b -> (a, b)
f aa bb = (aa, ida bb)
    where
        ida = id
