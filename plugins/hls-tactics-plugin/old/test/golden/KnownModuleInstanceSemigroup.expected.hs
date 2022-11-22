data Foo = Foo

instance Semigroup Foo where
  (<>) _ _ = Foo


data Bar = Bar Foo Foo

instance Semigroup Bar where
  (Bar foo foo') <> (Bar foo2 foo3)
    = Bar (foo <> foo2) (foo' <> foo3)

