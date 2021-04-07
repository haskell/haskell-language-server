data Mono = Monoid [String]

instance Semigroup Mono where
  (<>) = undefined

instance Monoid Mono where
  mempty = _

