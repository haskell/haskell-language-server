{-# LANGUAGE TypeFamilies #-}

type family Yo where
  Yo = Bool

test :: Yo -> Int
test False = _
test True = _

