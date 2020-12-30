{- | Multi line comments are parsed correctly.

 Note that if they open and close on a single line, their content is ignored.
-}
module TMulti () where

{- ORMOLU_DISABLE -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"

  {- >>> 3+3
    -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"

  {-| >>> IGNORED -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"
