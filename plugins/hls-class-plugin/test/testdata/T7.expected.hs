module T7 where

    data X = X

    class Test a where
      f :: a -> a
      g :: a
      h :: a -> a
      i :: a

    instance Test X where
      f X = X
      g = _
      h = _
      i = _




    whiteSpaceBeforeAndIndentationOfThisShouldBePreserved = ()
