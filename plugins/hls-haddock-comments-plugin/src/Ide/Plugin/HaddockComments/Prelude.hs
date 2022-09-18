{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ide.Plugin.HaddockComments.Prelude where
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint (AnnKey, Annotation)

data GenComments = GenComments
  { title      :: T.Text,
    updateAnns :: LHsDecl GhcPs -> TransformT Maybe ()
  }

-- | Defines how to generate haddock comments by tweaking annotations of AST
data GenCommentsSimple = forall a.
  GenCommentsSimple
  { title         :: T.Text,
    fromDecl      :: HsDecl GhcPs -> Maybe a,
    collectKeys   :: a -> [AnnKey],
    isFresh       :: Annotation -> Bool,
    updateAnn     :: Annotation -> Annotation,
    updateDeclAnn :: Annotation -> Annotation
  }
