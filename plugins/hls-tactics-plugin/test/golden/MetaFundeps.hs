{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

class Blah a b | a -> b, b -> a
instance Blah Int Bool

foo :: Int
foo = 10

bar :: Blah a b => a -> b
bar = undefined

qux :: Bool
qux = [wingman| use bar, use foo |]


