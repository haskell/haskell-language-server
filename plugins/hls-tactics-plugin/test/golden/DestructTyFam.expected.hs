{-# LANGUAGE TypeFamilies #-}

type family Yo where
  Yo = Bool

test :: Yo -> Int
test False = _w0
test True = _w1

