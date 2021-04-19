data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
   fmap fab (Leaf a) = Leaf (fab a)
   fmap fab (Branch tr' tr_a) = Branch (fmap fab tr') (fmap fab tr_a)
