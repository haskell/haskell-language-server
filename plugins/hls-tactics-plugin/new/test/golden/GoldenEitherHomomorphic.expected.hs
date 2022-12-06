eitherSplit :: a -> Either (a -> b) (a -> c) -> Either b c
eitherSplit a (Left fab) = Left (fab a)
eitherSplit a (Right fac) = Right (fac a)
