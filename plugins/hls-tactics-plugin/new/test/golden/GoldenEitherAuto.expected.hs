either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fac _ (Left a) = fac a
either' _ fbc (Right b) = fbc b
