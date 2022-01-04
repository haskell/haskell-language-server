{-# LANGUAGE TypeFamilies #-}

type family Yo where
  Yo = Bool

test :: Yo -> Int
test b = _

