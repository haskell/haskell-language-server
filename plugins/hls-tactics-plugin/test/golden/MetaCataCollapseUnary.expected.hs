import GHC.Generics

class Yo f where
    yo :: f x -> Int

instance (Yo f) => Yo (M1 _1 _2 f) where
  yo (M1 fx) = yo fx

