foo :: Bool -> _ -> () -> Int
foo True new_def = \() -> new_def [True]

foo False new_def = const 1