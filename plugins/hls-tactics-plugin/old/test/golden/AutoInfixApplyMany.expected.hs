test :: (a -> b -> x -> c) -> a -> (a -> b) -> x -> c
test (/:) a f x = (a /: f a) x

