-- The default language extensions for the eval plugin are the same as those for ghci

module TSameDefaultLanguageExtensionsAsGhci where

{-
Running `:showi language` within ghci currently lists NoDatatypeContexts, ExtendedDefaultRules, NoMonomorphismRestriction and NondecreasingIndentation.

The flags NoDatatypeContexts and NondecreasingIndentation are globally set in Haskell2021, whereas ExtendedDefaultRules and NoMonomorphismRestriction are set manually within ghci.
(see https://github.com/ghc/ghc/blob/5abf59976c7335df760e5d8609d9488489478173/ghc/GHCi/UI.hs#L473-L483)

It therefore suffices to test for ExtendedDefaultRules and NoMonomorphismRestriction only.
-}


-- ExtendedDefaultRules

-- >>> []
-- []

-- >>> reverse []
-- []

-- NoMonomorphismRestriction

-- >>> plus = (+)
-- >>> :t plus
-- plus :: Num a => a -> a -> a
