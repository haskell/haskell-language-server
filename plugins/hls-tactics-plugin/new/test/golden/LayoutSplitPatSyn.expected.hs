{-# LANGUAGE PatternSynonyms #-}

pattern JustSingleton :: a -> Maybe [a]
pattern JustSingleton a <- Just [a]


test :: Maybe [Bool] -> Maybe Bool
test (JustSingleton False) = _w0
test (JustSingleton True) = _w1


