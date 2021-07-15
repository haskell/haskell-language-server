{-# LANGUAGE PatternSynonyms #-}

pattern JustSingleton :: a -> Maybe [a]
pattern JustSingleton a <- Just [a]

amIASingleton :: Maybe [a] -> Maybe a
amIASingleton (JustSingleton a) = Just a

