{-# LANGUAGE PatternSynonyms #-}

pattern Blah :: a -> Maybe a
pattern Blah a = Just a

test :: Maybe Bool -> a
test (Blah a) = _

