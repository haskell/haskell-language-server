module NoteDef (foo) where

foo :: Int -> Int
foo _ = 0 -- We always return zero, see Note [Returning zero from foo]

{- Note [Returning zero from foo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a big long form note, with very important info

Note [Multiple notes in comment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is also a very common thing to do for GHC

-}
