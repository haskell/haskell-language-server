module TypedHoles2 (foo2) where
newtype A = A Int
foo2 :: [A] -> A
foo2 x = _
  where
    stuff (A a) = A (a + 1)
