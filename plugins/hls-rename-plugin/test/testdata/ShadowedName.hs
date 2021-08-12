foo :: Int -> Int
foo x = foo + 10
    where
        foo = 20
