{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

data GADT a where
  B1 :: GADT Bool
  B2 :: GADT Bool
  Int :: GADT Int
  Var :: GADT a


hasHomo :: GADT Bool -> GADT a
hasHomo g = _

cantHomo :: GADT a -> GADT Int
cantHomo g = _

hasHomoLam :: GADT Bool -> GADT a
hasHomoLam = _

cantHomoLam :: GADT a -> GADT Int
cantHomoLam = _

