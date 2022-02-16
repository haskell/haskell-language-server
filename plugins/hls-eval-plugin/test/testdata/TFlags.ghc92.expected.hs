-- Support for language options

{-# LANGUAGE ScopedTypeVariables #-}
module TFlags where

-- Language options set in the module source (ScopedTypeVariables)
-- also apply to tests so this works fine
-- >>> f = (\(c::Char) -> [c])

{- Multiple options can be set with a single `:set`

>>> :set -XMultiParamTypeClasses -XFlexibleInstances
>>> class Z a b c
-}

{-

Options apply only in the section where they are defined (unless they are in the setup section), so this will fail:

>>> class L a b c
Too many parameters for class `L'
(Enable MultiParamTypeClasses to allow multi-parameter classes)
In the class declaration for `L'
-}


{-
Options apply to all tests in the same section after their declaration.

Not set yet:

>>> class D
No parameters for class `D'
(Enable MultiParamTypeClasses to allow no-parameter classes)
In the class declaration for `D'

Now it works:

>>>:set -XMultiParamTypeClasses
>>> class C

It still works

>>> class F
-}

{- Now -package flag is handled correctly:

>>> :set -package ghc-prim
>>> import GHC.Prim

-}


{- Invalid option/flags are reported, but valid ones will be reflected

>>> :set -XRank2Types -XAbsent -XDatatypeContexts -XWrong -fprint-nothing-at-all
<interactive>: warning:
    -XDatatypeContexts is deprecated: It was widely considered a misfeature, and has been removed from the Haskell language.
Some flags have not been recognized: -XAbsent, -XWrong, -fprint-nothing-at-all

-}
