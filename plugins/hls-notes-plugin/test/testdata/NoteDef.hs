module NoteDef (foo) where

foo :: Int -> Int
foo _ = 0 -- We always return zero, see Note [Returning zero from foo]

-- The plugin is more liberal with the note definitions, see Note [Single line comments]
-- It does not work on wrong note definitions, see Note [Not a valid Note]

-- We can also have multiple references to the same note, see
-- Note [Single line comments]

{- Note [Returning zero from foo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a big long form note, with very important info

Note [Multiple notes in comment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is also a very common thing to do for GHC

-}

  -- Note [Single line comments]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- GHC's notes script only allows multiline comments to define notes, but in the
  -- HLS codebase this single line style can be found as well.

{- Note [Not a valid Note]

~~~~~~~~~~~~
The underline needs to be directly under the Note header
-}
