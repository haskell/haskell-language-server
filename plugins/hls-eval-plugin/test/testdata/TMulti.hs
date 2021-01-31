{- | Multi line comments are parsed correctly.
-}
module TMulti () where

{- ORMOLU_DISABLE -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"

  {- >>> 3+3
    -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"

  {-| >>> "NOT IGNORED" -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"
