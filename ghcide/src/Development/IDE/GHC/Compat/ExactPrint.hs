{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module Development.IDE.GHC.Compat.ExactPrint
    ( ExactPrint
    , exactPrint
    , makeDeltaAst
#if !MIN_VERSION_ghc(9,2,0)
    , Annotated(..)
#endif
    ) where

import           Language.Haskell.GHC.ExactPrint
import           Retrie.ExactPrint               (Annotated (..))

#if !MIN_VERSION_ghc(9,2,0)
class ExactPrint ast where
    makeDeltaAst :: ast -> ast
    makeDeltaAst = id

instance ExactPrint ast
#endif

