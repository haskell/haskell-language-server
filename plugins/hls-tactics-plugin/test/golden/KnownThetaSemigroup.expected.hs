data Semi a = Semi a

instance Semigroup a => Semigroup (Semi a) where
  (Semi a) <> (Semi a') = Semi (a <> a')

