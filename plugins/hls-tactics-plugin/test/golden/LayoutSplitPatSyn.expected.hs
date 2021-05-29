{-# LANGUAGE PatternSynonyms #-}

pattern JustSingleton :: a -> Maybe [a]
pattern JustSingleton a <- Just [a]


test :: Maybe [Bool] -> Maybe Bool
test (JustSingleton False) = _
test (JustSingleton True) = _


