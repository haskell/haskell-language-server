baz, bar :: Bool -> Int
foo :: Bool -> _ -> Int
bar = const 1
foo True new_def = new_def [True]

foo False new_def = 1
baz = 1