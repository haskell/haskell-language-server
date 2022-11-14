foo ::
  -- c1
  Bool -- c2
  -- c3
  -> -- c4
  -- | c5
  () -- c6
  -> Int
foo True () = new_def [True]

foo False () = 1