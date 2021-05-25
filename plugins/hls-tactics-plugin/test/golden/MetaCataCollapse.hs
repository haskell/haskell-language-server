{-# LANGUAGE TypeOperators #-}

import GHC.Generics

class Yo f where
    yo :: f x -> Int

instance (Yo f, Yo g) => Yo (f :*: g) where
  yo = [wingman| intros x, cata x, collapse |]

