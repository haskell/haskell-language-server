{-# LANGUAGE TypeFamilies #-}

data family Yo
data instance Yo = Heya Int

test :: Yo -> Int
test (Heya n) = _w0

