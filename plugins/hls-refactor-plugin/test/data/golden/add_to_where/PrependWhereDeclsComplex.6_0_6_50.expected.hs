foo True = new_def [True]
  where -- c1
        baz = 2 -- c2
        -- c3
foo False = False

bar True = new_def [True]
  where
  new_def = _
      -- c1baz = 2 -- c2
    -- c3
bar False = False