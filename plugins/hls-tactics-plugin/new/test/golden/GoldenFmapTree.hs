data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
   fmap = _
