reassoc :: (a, (b, c)) -> ((a, b), c)
reassoc (a, (b, c)) = ((a, b), c)
