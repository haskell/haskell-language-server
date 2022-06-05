module T15 where
import qualified T15A

class F a where
    f :: a

instance F T15A.A where
  f = undefined
