data Foo = A Int | B Bool | C

-- Make sure we don't shadow the i and b bindings when we empty case
-- split
foo :: Int -> Bool -> Foo -> ()
foo i b x = case x of
  A n -> _
  B b' -> _
  C -> _

