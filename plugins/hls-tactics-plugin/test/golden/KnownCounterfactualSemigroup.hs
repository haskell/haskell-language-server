{-# LANGUAGE UndecidableInstances #-}

data Semi = Semi [String] Int

instance Semigroup Int => Semigroup Semi where
  (<>) = _

