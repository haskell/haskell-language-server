{-# LANGUAGE GADTs #-}

-- Should suggest Left and Right, but not []
t1 :: Either a b
t1 = _


data ManyConstructors = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10

noCtorsIfMany :: ManyConstructors
noCtorsIfMany = _


data GADT a where
  IntGADT  :: GADT Int
  BoolGADT :: GADT Bool
  VarGADT  :: GADT a

gadtCtor :: GADT Int
gadtCtor = _

