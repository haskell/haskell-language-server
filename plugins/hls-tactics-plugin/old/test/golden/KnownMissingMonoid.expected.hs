data Mono a = Monoid [String] a

instance Semigroup (Mono a) where
  (<>) = undefined

instance Monoid (Mono a) where
  mempty = Monoid mempty _w0

