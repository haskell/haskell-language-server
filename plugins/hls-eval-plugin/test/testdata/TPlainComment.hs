{- |
Tests in plain comments in both single line or multi line format.
Tests are ignored if:
    * do not start on the first column
    * are in multi line comments that open and close on the same line
-}
module TPlain where

{- ORMOLU_DISABLE -}

-- Single line comment
-- >>> "a"++"b"

{- Multi line comment

>>> "b"++"c"
-}

{- >>> 2+five

   ^-- This works, as it starts at the first column after the header.

     >>> IGNORED as it does not start on the first column
-}
five = 5
