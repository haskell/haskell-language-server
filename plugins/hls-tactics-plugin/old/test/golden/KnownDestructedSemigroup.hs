data Test a = Test [a]

instance Semigroup (Test a) where
  Test a <> Test c = _

