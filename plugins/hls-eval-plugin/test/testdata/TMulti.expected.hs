{- | Multi line comments are parsed correctly.
-}
module TMulti () where

{- ORMOLU_DISABLE -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"
-- "ab"

  {- >>> 3+3
6
    -}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"
-- "ab"

  {-| >>> "NOT IGNORED" 
"NOT IGNORED"
-}

-- this should work fine if previous multi comment is parsed correctly
-- >>> "a"++"b"
-- "ab"
