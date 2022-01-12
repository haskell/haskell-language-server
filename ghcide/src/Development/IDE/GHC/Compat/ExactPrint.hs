{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains compatibility constructs to write type signatures across
--   multiple ghc-exactprint versions, accepting that anything more ambitious is
--   pretty much impossible with the GHC 9.2 redesign of ghc-exactprint
module Development.IDE.GHC.Compat.ExactPrint
    ( ExactPrint
    , exactPrint
    , makeDeltaAst
#if !MIN_VERSION_ghc(9,2,0)
    , Annotated(..)
#endif
    ) where

import           Language.Haskell.GHC.ExactPrint
#if !MIN_VERSION_ghc(9,2,0)
import           Retrie.ExactPrint               (Annotated (..))
#endif

#if !MIN_VERSION_ghc(9,2,0)
class ExactPrint ast where
    makeDeltaAst :: ast -> ast
    makeDeltaAst = id

instance ExactPrint ast
#endif

