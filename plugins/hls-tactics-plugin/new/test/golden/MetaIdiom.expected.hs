foo :: Int -> Int -> Int
foo = undefined

test :: Maybe Int
test = (foo <$> _w0) <*> _w1

