{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T23 where
import Data.Proxy      (Proxy (..))
import GHC.TypeNats    (KnownNat)
import Type.Reflection (Typeable)

f :: forall k n a. (KnownNat k, KnownNat n, Typeable a)
  => Proxy k -> Proxy n -> Proxy a -> ()
f _ _ _ = ()

-- >>> :type f
-- f :: forall k1 (k2 :: Nat) (n :: Nat) (a :: k1).
--      (KnownNat k2, KnownNat n, Typeable a) =>
--      Proxy k2 -> Proxy n -> Proxy a -> ()
