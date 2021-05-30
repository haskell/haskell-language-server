data Pair a b = Pair {pa :: a, pb :: b}

p :: Pair (a -> a) (a -> b -> c -> b)
p = Pair {pa = _, pb = _}

