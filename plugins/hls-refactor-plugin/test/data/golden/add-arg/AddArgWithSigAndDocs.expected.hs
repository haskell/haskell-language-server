foo ::
  -- c1
  Bool -- c2
  -- c3
  -> -- c4
  -- | c5
  () -- c6
  -> _ -> Int
foo True () new_def = new_def [True]

foo False () new_def = 1