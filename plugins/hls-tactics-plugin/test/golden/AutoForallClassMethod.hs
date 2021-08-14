{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Functor.Contravariant

class Semigroupal cat t1 t2 to f where
  combine :: cat (to (f x y) (f x' y')) (f (t1 x x') (t2 y y'))

comux :: forall p a b c d. Semigroupal Op (,) (,) (,) p => p (a, c) (b, d) -> (p a b, p c d)
comux = _

