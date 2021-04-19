foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ b [] = b
foldr2 fabb b (a : as') = fabb a (foldr2 fabb b as')
