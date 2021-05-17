{- |
Tests in plain comments in both single line or multi line format, both forward and backward.
Tests are ignored if:
    * do not start on the first column (in Ordinary Haskell)
    * do not start on the first or second column (in Literate Haskell)
-}
module THaddock () where

{- ORMOLU_DISABLE -}

-- | Single line comment
-- >>> "a"++"b"

{- | Multi line comment

>>> "b"++"c"
-}

double :: Num a => a -> a
double a = a + a
-- ^ Single line backward comments
-- >>> double 11

twice :: [a] -> [a]
twice a = a ++ a
{- ^ Multi-line backward comments
>>> twice "ABC"
-}

{- | >>> 2+five

   ^-- This works, as it starts at the first column after the header.

     >>> IGNORED as it does not start on the first column
-}
five :: Integer
five = 5
