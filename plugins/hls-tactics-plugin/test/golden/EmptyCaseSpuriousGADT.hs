{-# LANGUAGE GADTs #-}

data Foo a where
  Foo :: Foo Int

foo :: Foo Bool -> ()
foo x = case x of

