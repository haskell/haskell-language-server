foo :: forall a. (a -> a) -> a -> a
foo f x = f $ x
