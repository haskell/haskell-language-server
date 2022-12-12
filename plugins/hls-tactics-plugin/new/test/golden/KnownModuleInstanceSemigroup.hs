data Foo = Foo

instance Semigroup Foo where
  (<>) _ _ = Foo


data Bar = Bar Foo Foo

instance Semigroup Bar where
  (<>) = _

