data Semi a = Semi a

instance Semigroup a => Semigroup (Semi a) where
  (<>) = _

