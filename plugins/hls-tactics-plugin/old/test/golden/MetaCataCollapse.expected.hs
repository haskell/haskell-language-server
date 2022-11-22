{-# LANGUAGE TypeOperators #-}

import GHC.Generics

class Yo f where
    yo :: f x -> Int

instance (Yo f, Yo g) => Yo (f :*: g) where
  yo (fx :*: gx)
    = let
        fx_c = yo fx
        gx_c = yo gx
      in _w0 fx_c gx_c

