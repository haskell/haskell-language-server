-- | This module contains compatibility constructs to write type signatures across
--   multiple ghc-exactprint versions, accepting that anything more ambitious is
--   pretty much impossible with the GHC 9.2 redesign of ghc-exactprint
module Development.IDE.GHC.Compat.ExactPrint
    ( ExactPrint
    , exactPrint
    , makeDeltaAst
    , Retrie.Annotated, pattern Annotated, astA, annsA
    ) where

import           Development.IDE.GHC.Compat.Parser
import           Language.Haskell.GHC.ExactPrint   as Retrie
import qualified Retrie.ExactPrint                 as Retrie


pattern Annotated :: ast -> ApiAnns -> Retrie.Annotated ast
pattern Annotated {astA, annsA} <- ((,()) . Retrie.astA -> (astA, annsA))
