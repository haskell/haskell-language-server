{-# LANGUAGE UndecidableInstances #-}

data Semi = Semi [String] Int

instance Semigroup Int => Semigroup Semi where
  (Semi ss n) <> (Semi strs i) = Semi (ss <> strs) (n <> i)

