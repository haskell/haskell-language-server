foo :: Bool -> _ -> Int
foo True new_def =
  let bar = new_def
  in bar

foo False new_def = 1