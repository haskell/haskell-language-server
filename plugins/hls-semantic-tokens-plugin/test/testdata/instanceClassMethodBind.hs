module InstanceClassMethodBind where


data Foo = Foo Int
instance Eq Foo where
    (==) = undefined
