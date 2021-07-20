{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T21 where
import Data.Proxy (Proxy(..))
import GHC.TypeNats (KnownNat)
import Type.Reflection (Typeable)

fun :: forall k n a. (KnownNat k, KnownNat n, Typeable a)
    => Proxy k -> Proxy n -> Proxy a -> ()
fun _ _ _ = ()

