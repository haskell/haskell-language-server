-- | This module contains compatibility constructs to write type signatures across
--   multiple ghc-exactprint versions, accepting that anything more ambitious is
--   pretty much impossible with the GHC 9.2 redesign of ghc-exactprint
module Development.IDE.GHC.Compat.ExactPrint
    ( ExactPrint
    , exactPrint
    , makeDeltaAst
    , Retrie.Annotated, pattern Annotated, astA, annsA
    ) where

#if !MIN_VERSION_ghc(9,2,0)
import           Control.Arrow                     ((&&&))
#else
import           Development.IDE.GHC.Compat.Parser
#endif
import           Language.Haskell.GHC.ExactPrint   as Retrie
import qualified Retrie.ExactPrint                 as Retrie

#if !MIN_VERSION_ghc(9,2,0)
class ExactPrint ast where
    makeDeltaAst :: ast -> ast
    makeDeltaAst = id

instance ExactPrint ast
#endif

#if !MIN_VERSION_ghc(9,2,0)
pattern Annotated :: ast -> Anns -> Retrie.Annotated ast
pattern Annotated {astA, annsA} <- (Retrie.astA &&& Retrie.annsA -> (astA, annsA))
#else
pattern Annotated :: ast -> ApiAnns -> Retrie.Annotated ast
pattern Annotated {astA, annsA} <- ((,()) . Retrie.astA -> (astA, annsA))
#endif
