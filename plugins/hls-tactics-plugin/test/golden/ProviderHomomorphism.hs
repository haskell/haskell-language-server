{-# LANGUAGE GADTs #-}

data GADT a where
  B1 :: GADT Bool
  B2 :: GADT Bool
  Int :: GADT Int
  Var :: GADT a


hasHomo :: GADT Bool -> GADT a
hasHomo g = _

cantHomo :: GADT a -> GADT Int
cantHomo g = _

