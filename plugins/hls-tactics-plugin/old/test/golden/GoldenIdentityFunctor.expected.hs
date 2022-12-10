data Ident a = Ident a
instance Functor Ident where
   fmap fab (Ident a) = Ident (fab a)
