data Set a = Set

instance Semigroup (Set a) where
  (<>) = _

instance Monoid (Set a) where
