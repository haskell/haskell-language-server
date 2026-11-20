module InsertPragmaOnce where

data A aaa

instance Functor A where
  fmap = undefined

instance Applicative A where
  pure = undefined
  (*>) = undefined
