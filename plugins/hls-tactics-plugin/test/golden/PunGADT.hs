{-# LANGUAGE GADTs #-}

data GADT a where
  GADT ::
    { blah :: Int
    , bar :: a
    } -> GADT a


split :: GADT a -> a
split x = _

