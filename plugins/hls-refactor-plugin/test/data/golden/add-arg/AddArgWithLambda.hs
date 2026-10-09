foo :: Bool -> () -> Int
foo True = \() -> new_def [True]

foo False = const 1