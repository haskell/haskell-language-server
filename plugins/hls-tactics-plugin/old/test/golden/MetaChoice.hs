reassoc :: (a, (b, c)) -> ((a, b), c)
reassoc (a, (b, c)) = [wingman| split; split | assume c; assume a | assume b |]
