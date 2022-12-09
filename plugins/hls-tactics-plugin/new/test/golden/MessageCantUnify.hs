{-# LANGUAGE DataKinds, GADTs #-}

data Z ab where
  Z :: (a -> b) -> Z '(a, b)

test :: Z ab
test = _

