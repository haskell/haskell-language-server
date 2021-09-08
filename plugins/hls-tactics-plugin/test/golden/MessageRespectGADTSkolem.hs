{-# LANGUAGE GADTs #-}

data Foo a where
  Foo :: Foo Int

test :: Foo as
test = _

