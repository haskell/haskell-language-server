{-# LANGUAGE PatternSynonyms #-}

data Test = Foo | Bar

pattern Fog :: Test
pattern Fog = Foo

pattern Bard :: Test
pattern Bard = Bar

{-# COMPLETE Fog, Bard #-}

test :: Test -> ()
test t = _

