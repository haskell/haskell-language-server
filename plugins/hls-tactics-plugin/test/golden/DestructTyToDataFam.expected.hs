{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

type family T1 a where
  T1 a = T2 Int

type family T2 a
type instance T2 Int = T3

type family T3 where
  T3 = Yo

data family Yo
data instance Yo = Heya Int

test :: T1 Bool -> Int
test (Heya n) = _w0

