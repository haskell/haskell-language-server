module T7 where

    data X = X

    class Test a where
      f :: a -> a
      g :: a

    instance Test X where
       f X = X
       g = _




    whiteSpaceBeforeAndIndentationOfThisShouldBePreserved = ()
