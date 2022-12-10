type FunctionTySyn = Bool -> Int
foo :: FunctionTySyn -> () -> Int
foo True () = new_def [True]

foo False () = 1