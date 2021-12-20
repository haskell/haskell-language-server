test :: (a -> b -> c) -> (c -> d -> e) -> a -> (a -> b) -> d -> e
test (/:) (-->) a f x = _
