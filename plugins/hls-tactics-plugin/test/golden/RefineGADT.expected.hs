{-# LANGUAGE GADTs #-}

data GADT a where
  One :: (b -> Int) -> GADT Int
  Two :: GADT Bool

test :: z -> GADT Int
test z = One _w0

