foo :: Bool -> _ -> Int
foo True new_def = bar
  where
    bar = new_def

foo False new_def = 1