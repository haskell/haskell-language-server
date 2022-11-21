test :: (a -> b -> c) -> a -> (a -> b) -> c
test (/:) a f = a /: f a

