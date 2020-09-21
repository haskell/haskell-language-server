module T22 where
import Data.Proxy      (Proxy (..))
import GHC.TypeNats    (KnownNat)
import Type.Reflection (Typeable)

f :: Integer
f = 32

-- >>> :t f
