{-# LANGUAGE TemplateHaskell #-}

-- | This module exists to circumvent a compile time exception on Windows with
-- Stack and GHC 8.10.1. It's just been pulled out from Development.IDE.Session.
-- See https://github.com/haskell/ghcide/pull/697
module Development.IDE.Session.VersionCheck (ghcVersionChecker) where

import           GHC.Check
-- Only use this for checking against the compile time GHC libDir!
-- Use getRuntimeGhcLibDir from hie-bios instead for everything else
-- otherwise binaries will not be distributable since paths will be baked into them
import qualified GHC.Paths

ghcVersionChecker :: GhcVersionChecker
ghcVersionChecker = $$(makeGhcVersionChecker (return GHC.Paths.libdir))
