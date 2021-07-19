{-# LANGUAGE GADTs #-}

data GADT a where
  GADT ::
    { blah :: Int
    , bar :: a
    } -> GADT a
  Bar ::
    { zoo :: Bool
    , baxter :: a
    , another :: a
    } -> GADT Bool
  Baz :: GADT Int


split :: GADT Bool -> a
split GADT {blah, bar} = _w0
split Bar {zoo, baxter, another} = _w1

