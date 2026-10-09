{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T23 where
import Data.Proxy      (Proxy (..))
import GHC.TypeNats    (KnownNat)
import Type.Reflection (Typeable)

f :: forall k n a. (KnownNat k, KnownNat n, Typeable a)
  => Proxy k -> Proxy n -> Proxy a -> ()
f _ _ _ = ()

