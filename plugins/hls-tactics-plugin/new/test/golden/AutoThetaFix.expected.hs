{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

data Fix f a = Fix (f (Fix f a))

instance ( Functor f
           -- FIXME(sandy): Unfortunately, the recursion tactic fails to fire
           -- on this case. By explicitly adding the @Functor (Fix f)@
           -- dictionary, we can get Wingman to generate the right definition.
         , Functor (Fix f)
         ) => Functor (Fix f) where
  fmap fab (Fix f) = Fix (fmap (fmap fab) f)

